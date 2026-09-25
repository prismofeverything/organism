//! Move server: one process holds a trained network and answers positions.
//!
//! The existing stdin protocol in `main` is a rules oracle — it reports legal
//! actions but never consults a network. This adds the other half, so a trained
//! model can play as a bot. Weights are loaded once at startup; loading 2.36M
//! parameters per move would dominate the cost of answering one.
//!
//! Line-delimited JSON, one response per request. A position arrives either as
//! the moves that produced it or as the position itself:
//!   {"actions":[12,27,...]}            -> replay from the opening
//!   {"position":{...}}                 -> a board handed over directly
//!   {"action":27,"legal":[...],...}    -> the move to play next
//!
//! The website keeps an undo stack of whole states rather than a choice
//! history, so a live game can only offer the second form. Adding `"echo":true`
//! returns the position alongside the move, which is how a caller written in
//! another language checks its encoding against this one, and `"next"`, the
//! position the move leads to. A caller needs `next` where its own engine
//! decomposes a turn differently: the Clojure engine funds a growth in one
//! choice, while this one picks donor spaces a food at a time, so the caller
//! replays those sub-decisions here and reads the finished allocation off the
//! action it lands on.
//!
//! Action indices match the Clojure and Python engines exactly; that agreement
//! is what `tests/check_native_parity.py` verifies.
use crate::{
    game::{Board, Rules, State},
    network::Network,
    search::{self, Limits, Random, Timings, Tree},
};
use crate::args::argument;
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::io::{self, BufRead, Write};
use std::time::Instant;

pub fn main(args: &[String]) -> Result<()> {
    let players: usize = argument(args, "--players", "3").parse()?;
    let rings: usize = argument(args, "--rings", "4").parse()?;
    let blocks: usize = argument(args, "--blocks", "8").parse()?;
    let filters: i64 = argument(args, "--filters", "128").parse()?;
    let sims: usize = argument(args, "--sims", "64").parse()?;
    let threads: usize = argument(args, "--threads", "4").parse()?;
    let batch: usize = argument(args, "--gpu-batch", "16").parse()?;
    let max_steps: usize = argument(args, "--max-steps", "4000").parse()?;
    let repetition: u32 = argument(args, "--repetition", "3").parse()?;
    let weights = argument(args, "--weights", "");
    anyhow::ensure!(!weights.is_empty(), "--weights is required");
    anyhow::ensure!(sims > 0 && threads > 0 && batch > 0, "invalid budget");

    // A served model plays the rules it was trained on. Defaults match the
    // launcher, so a bot cannot silently offer a different game than it learned.
    let rules = Rules {
        eat_threshold: argument(args, "--eat-threshold", "5").parse()?,
        require_useful_action: argument(args, "--require-useful-action", "1").parse::<u8>()? != 0,
        sacrifice_yields_nothing: argument(args, "--sacrifice-yields-nothing", "1").parse::<u8>()? != 0,
        elide_forced: argument(args, "--elide-forced", "0").parse::<u8>()? != 0,
    };
    let stall_limit: u32 = argument(args, "--stall-limit", "15").parse()?;

    tch::set_num_threads(threads as i32);
    tch::set_num_interop_threads(1);
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()
        .ok();
    let device = if args.iter().any(|a| a == "--cpu") {
        tch::Device::Cpu
    } else {
        anyhow::ensure!(tch::Cuda::is_available(), "CUDA unavailable; pass --cpu");
        tch::Device::Cuda(0)
    };

    let board = Board::new(players, rings, false).with_rules(rules);
    let mut net = Network::new(
        players,
        board.grid(),
        board.action_size(),
        blocks,
        filters,
        device,
    );
    net.vs
        .load(&weights)
        .with_context(|| format!("loading {weights} as a {blocks}x{filters} network"))?;
    net.vs.freeze();
    eprintln!(
        "organism serve: {players}p rings={rings} {blocks}x{filters} sims={sims} device={device:?} rules={rules:?} stall_limit={stall_limit}"
    );

    let mut rng = Random(argument(args, "--seed", "17").parse()?);
    let stdout = io::stdout();
    for line in io::stdin().lock().lines() {
        let started = Instant::now();
        let request: Value = serde_json::from_str(&line?)?;
        // `steps` feeds the search's horizon limits. A handed-over position has
        // no move count of its own, so estimate it from the round: too low
        // merely means the cutoff arrives later than it would in training.
        let (mut state, mut steps) = (board.initial(), 0usize);
        if request.get("position").is_some_and(|p| !p.is_null()) {
            state = serde_json::from_value::<State>(request["position"].clone())
                .map_err(|e| anyhow::anyhow!("position is not a board: {e}"))?;
            anyhow::ensure!(
                state.pieces.len() == board.spaces.len()
                    && state.food.len() == board.spaces.len()
                    && state.captures.len() == players,
                "position is shaped for a different board than {players}p rings={rings}"
            );
            steps = state.round as usize * players * 6;
        } else {
            for action in request["actions"].as_array().into_iter().flatten() {
                let action = action.as_u64().context("action must be an integer")? as usize;
                state = board
                    .legal(&state)
                    .into_iter()
                    .find(|(a, _)| *a == action)
                    .with_context(|| format!("illegal action {action} at step {steps}"))?
                    .1;
                steps += 1;
            }
        }
        let echo = request["echo"].as_bool().unwrap_or(false);
        let legal: Vec<usize> = board.legal(&state).into_iter().map(|(a, _)| a).collect();
        let mut out = stdout.lock();
        if legal.is_empty() || state.winner.is_some() {
            let mut done = json!({"action":Value::Null,"legal":legal,
                                  "winner":state.winner,"reason":"terminal"});
            if echo {
                done["position"] = serde_json::to_value(&state)?;
            }
            writeln!(out, "{done}")?;
            out.flush()?;
            continue;
        }
        // One legal action is not a decision; answer it without paying for search.
        let (action, policy) = if legal.len() == 1 {
            (legal[0], vec![])
        } else {
            let budget: usize = request["sims"].as_u64().map_or(sims, |v| v as usize);
            let mut trees = vec![Tree::new(state.clone(), players)];
            let limits = vec![Limits {
                seen: None,
                steps,
                max_steps,
                repetition,
                bootstrap_horizon: false,
            }];
            search::queued(
                &board,
                &mut trees,
                &limits,
                &net,
                budget,
                &mut rng,
                false,
                batch,
                || Ok(()),
                &mut Timings::default(),
            )?;
            let pi = trees[0].policy(&board);
            let best = pi
                .iter()
                .enumerate()
                .filter(|(a, _)| legal.contains(a))
                .max_by(|a, b| a.1.total_cmp(b.1))
                .map(|(a, _)| a)
                .unwrap_or(legal[0]);
            (best, pi)
        };
        let advanced = board
            .legal(&state)
            .into_iter()
            .find(|(a, _)| *a == action)
            .map(|(_, s)| s);
        let visits: Vec<Value> = legal
            .iter()
            .map(|&a| json!([a, policy.get(a).copied().unwrap_or(0.)]))
            .collect();
        let mut answer = json!({"action":action,"legal":legal,"visits":visits,
                                "player":state.player,"round":state.round,
                                "phase":board.phase(&state),
                                "seconds":started.elapsed().as_secs_f64()});
        if echo {
            answer["position"] = serde_json::to_value(&state)?;
            answer["next"] = serde_json::to_value(&advanced)?;
        }
        writeln!(out, "{answer}")?;
        out.flush()?;
    }
    Ok(())
}
