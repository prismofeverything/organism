//! Fixed-opponent, seat-balanced evaluation; evaluation games never enter replay.
use crate::{
    game::{Board, State},
    network::Network,
    search::{self, Random},
};
use anyhow::Result;
use serde_json::{Value, json};
use std::{collections::HashMap, time::Instant};

pub fn run<
    F: FnMut() -> Result<()>,
    O: FnMut(&[State], &[usize], &[Option<Value>]) -> Result<()>,
>(
    board: &Board,
    candidate: &Network,
    opponent: &Network,
    sims: usize,
    per_seat: usize,
    max_steps: usize,
    repetition: u32,
    seed: u64,
    mut control: F,
    mut observe: O,
) -> Result<Value> {
    let start = Instant::now();
    let count = board.players * per_seat;
    let mut states = vec![board.initial(); count];
    let mut steps = vec![0usize; count];
    let mut seen: Vec<HashMap<State, u32>> = vec![HashMap::new(); count];
    let mut results: Vec<Option<Value>> = vec![None; count];
    let mut rngs: Vec<_> = (0..count).map(|i| Random(seed + i as u64)).collect();
    loop {
        control()?;
        for i in 0..count {
            if results[i].is_some() {
                continue;
            }
            let key = crate::train::repetition_state(&states[i]);
            let visits = seen[i].get(&key).copied().unwrap_or(0);
            let reason = if states[i].winner.is_some() {
                Some("win")
            } else if steps[i] >= max_steps {
                Some("max_steps")
            } else if repetition > 0 && visits >= repetition - 1 {
                Some("repetition")
            } else {
                None
            };
            if let Some(reason) = reason {
                results[i] = Some(
                    json!({"seat":i%board.players,"seed":seed+i as u64,"winner":states[i].winner,
                    "candidate_won":states[i].winner == Some(i%board.players),"termination":reason,"steps":steps[i]}),
                );
            }
        }
        observe(&states, &steps, &results)?;
        if results.iter().all(Option::is_some) {
            break;
        }
        // Partition by whose turn it is, so each network evaluates a useful batch.
        let turns: Vec<_> = states.iter().map(|s| s.player).collect();
        for is_candidate in [true, false] {
            let active: Vec<_> = (0..count)
                .filter(|&i| {
                    results[i].is_none() && (turns[i] == i % board.players) == is_candidate
                })
                .collect();
            if active.is_empty() {
                continue;
            }
            let batch: Vec<_> = active.iter().map(|&i| states[i].clone()).collect();
            let policies = search::policies(
                board,
                &batch,
                if is_candidate { candidate } else { opponent },
                sims,
                &mut Random(0),
                false,
                &mut control,
            )?;
            for (&i, pi) in active.iter().zip(policies) {
                *seen[i]
                    .entry(crate::train::repetition_state(&states[i]))
                    .or_default() += 1;
                // Fixed per-game seeds diversify openings without search-root noise.
                let action = if steps[i] < 30 {
                    rngs[i].sample(&pi)
                } else {
                    pi.iter()
                        .enumerate()
                        .max_by(|a, b| a.1.total_cmp(b.1))
                        .unwrap()
                        .0
                };
                states[i] = board
                    .legal(&states[i])
                    .into_iter()
                    .find(|(a, _)| *a == action)
                    .unwrap()
                    .1;
                steps[i] += 1;
            }
        }
    }
    let games: Vec<_> = results.into_iter().flatten().collect();
    let wins = games.iter().filter(|g| g["candidate_won"] == true).count();
    let cutoffs = games.iter().filter(|g| g["termination"] != "win").count();
    Ok(
        json!({"games":games,"wins":wins,"losses":count-wins-cutoffs,"cutoffs":cutoffs,
        "win_rate_all_games":wins as f64/count as f64,"games_per_seat":per_seat,"simulations":sims,
        "max_steps":max_steps,"repetition":repetition,"seed":seed,"opening_sampling_decisions":30,
        "seconds":start.elapsed().as_secs_f64()}),
    )
}
