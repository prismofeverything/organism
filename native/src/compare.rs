//! Isolated, resumable comparison of immutable checkpoint weights.
use crate::{evaluate::Session, game::Board, network::Network, train::Config};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::{
    fs::{self, File},
    io::BufReader,
    path::PathBuf,
};

pub fn main(args: &[String]) -> Result<()> {
    let manifest = PathBuf::from(
        args.get(2)
            .context("compare requires a manifest JSON path")?,
    );
    let spec: Value = serde_json::from_reader(BufReader::new(File::open(&manifest)?))?;
    let output = manifest.parent().context("manifest directory")?;
    let config: Config = serde_json::from_value(spec["config"].clone())?;
    anyhow::ensure!(
        (2..=3).contains(&config.players),
        "comparison supports two or three players"
    );
    let board = Board::new(config.players, config.rings, false);
    tch::set_num_threads(2);
    tch::set_num_interop_threads(1);
    rayon::ThreadPoolBuilder::new()
        .num_threads(2)
        .build_global()?;
    let device = if args.iter().any(|x| x == "--cpu") {
        tch::Device::Cpu
    } else {
        tch::Device::Cuda(0)
    };
    let mut nets = vec![];
    for key in ["candidate", "opponent"] {
        let mut net = Network::new(
            config.players,
            board.grid(),
            board.action_size(),
            config.blocks,
            config.filters,
            device,
        );
        net.vs
            .load(output.join(spec[key]["weights"].as_str().context("weight path")?))?;
        net.vs.freeze();
        nets.push(net);
    }
    let state_path = output.join("session.json");
    let mut session = if state_path.exists() {
        let saved: Value = serde_json::from_reader(BufReader::new(File::open(&state_path)?))?;
        anyhow::ensure!(
            saved["manifest"] == spec,
            "comparison manifest changed; use a new output directory"
        );
        serde_json::from_value(saved["session"].clone())?
    } else {
        let mut s = Session::new(
            &board,
            spec["simulations"].as_u64().context("simulations")? as usize,
            spec["games_per_seat"].as_u64().context("games_per_seat")? as usize,
            config.max_steps,
            config.repetition,
            spec["seed"].as_u64().context("seed")?,
            10,
        );
        anyhow::ensure!(
            matches!(spec["cutoff_value"].as_str(), Some("mask" | "draw")),
            "invalid cutoff mode"
        );
        s.bootstrap_horizon = spec["cutoff_value"] == "mask";
        s.mixed(
            &board,
            vec![
                spec["opponent"]["identity"]
                    .as_str()
                    .context("opponent identity")?
                    .into(),
            ],
        );
        s
    };
    let mut ticks = 0;
    loop {
        if session.done() || ticks % 20 == 0 || output.join("STOP").exists() {
            let saved = json!({"manifest":spec,"session":session});
            let tmp = output.join("session.tmp.json");
            serde_json::to_writer(File::create(&tmp)?, &saved)?;
            fs::rename(tmp, &state_path)?;
            let mut report = session.report();
            report["manifest"] = spec.clone();
            report["complete"] = json!(session.done());
            report["completed_games"] = json!(session.results.iter().flatten().count());
            report["total_choices"] = json!(session.steps.iter().sum::<usize>());
            let tmp = output.join("report.tmp.json");
            serde_json::to_writer_pretty(File::create(&tmp)?, &report)?;
            fs::rename(tmp, output.join("report.json"))?;
        }
        if session.done() || output.join("STOP").exists() {
            break;
        }
        session.tick(&board, &nets[0], &nets[1], 8, || Ok(()))?;
        ticks += 1;
    }
    println!(
        "Comparison progress: {}",
        output.join("report.json").display()
    );
    Ok(())
}
