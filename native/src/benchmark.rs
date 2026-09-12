//! Wall-clock attribution includes GPU result synchronization; it is not kernel-only timing.
use crate::{
    game::{Board, State},
    network::Network,
    search::{self, Random, Timings},
    train::Config,
};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::{fs::File, path::PathBuf};
pub fn main(args: &[String]) -> Result<()> {
    tch::set_num_threads(2);
    tch::set_num_interop_threads(1);
    let source = PathBuf::from(
        args.get(2)
            .context("benchmark requires fixture JSON or model directory")?,
    );
    let (board, mut net, states) = if source.is_dir() {
        let c: Config = serde_json::from_reader(std::io::BufReader::new(File::open(
            source.join("config.json"),
        )?))?;
        let latest: Value = serde_json::from_reader(std::io::BufReader::new(File::open(
            source.join("latest.json"),
        )?))?;
        let generation = source.join("snapshots").join(
            latest["generation"]
                .as_str()
                .context("missing generation")?,
        );
        let saved: Value = serde_json::from_reader(std::io::BufReader::new(File::open(
            generation.join("state.json"),
        )?))?;
        let b = Board::new(c.players, c.rings, false);
        let mut states: Vec<State> = vec![];
        for e in saved["episodes"].as_array().context("episodes")? {
            let s: State = serde_json::from_value(e["state"].clone())?;
            if s.winner.is_none() {
                states.push(s)
            }
        }
        let replay = saved["replay"].as_array().context("replay")?;
        for sample in replay.iter().step_by((replay.len() / 16).max(1)) {
            let s: State = serde_json::from_value(sample["state"].clone())?;
            if s.winner.is_none() {
                states.push(s)
            }
        }
        let mut n = Network::new(
            c.players,
            b.grid(),
            b.action_size(),
            c.blocks,
            c.filters,
            tch::Device::Cuda(0),
        );
        n.vs.load(generation.join("model.ot"))?;
        println!(
            "{}",
            json!({"fixture_generation":generation,"sampled_positions":states.len(),"players":c.players,"note":"live trainer remains running; inference time includes transfers and synchronization"})
        );
        (b, n, states)
    } else {
        let r: Value = serde_json::from_reader(std::io::BufReader::new(File::open(source)?))?;
        let b = Board::new(2, 4, false);
        let mut n = Network::new(2, b.grid(), b.action_size(), 4, 64, tch::Device::Cuda(0));
        n.vs.load(r["weights"].as_str().context("weights")?)?;
        let mut states = vec![];
        for path in r["paths"].as_array().context("paths")? {
            let mut s = b.initial();
            for a in path.as_array().context("path")? {
                s = b
                    .legal(&s)
                    .into_iter()
                    .find(|(i, _)| Some(*i as u64) == a.as_u64())
                    .context("illegal action")?
                    .1;
            }
            states.push(s);
        }
        (b, n, states)
    };
    net.vs.freeze();
    anyhow::ensure!(!states.is_empty(), "empty positions");
    // Fixed 64-position workload for every case; rotate case order on the second round.
    let work: Vec<_> = (0..64).map(|i| states[i % states.len()].clone()).collect();
    let threads: usize = args.get(3).map(|s| s.parse()).transpose()?.unwrap_or(4);
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()?;
    // Warm up each shape outside the measurements.
    for batch in [8, 16, 32, 64] {
        search::policies(
            &board,
            &work[..batch],
            &net,
            64,
            &mut Random(7),
            false,
            || Ok(()),
        )?;
    }
    for round in 0..2 {
        let mut sizes = vec![8, 16, 32, 64];
        if round == 1 {
            sizes.reverse()
        }
        for batch in sizes {
            let mut timing = Timings::default();
            for chunk in work.chunks(batch) {
                if args.iter().any(|a| a == "--queued") {
                    let mut trees: Vec<_> = chunk
                        .iter()
                        .map(|s| search::Tree::new(s.clone(), board.players))
                        .collect();
                    search::queued(
                        &board,
                        &mut trees,
                        &vec![search::Limits::default(); chunk.len()],
                        &net,
                        64,
                        &mut Random(7),
                        false,
                        (batch / 2).max(1),
                        || Ok(()),
                        &mut timing,
                    )?;
                } else {
                    search::policies_profiled(
                        &board,
                        chunk,
                        &net,
                        64,
                        &mut Random(7),
                        false,
                        || Ok(()),
                        &mut timing,
                    )?;
                }
            }
            println!(
                "{}",
                json!({"queued":args.iter().any(|a|a=="--queued"),"threads_requested":threads,"round":round,"batch":batch,"decisions":64,"decisions_per_second":64./timing.total_seconds,"timings":timing})
            );
        }
    }
    Ok(())
}
