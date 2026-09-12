use crate::{
    game::Board,
    network::Network,
    search::{self, Random},
};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::{fs::File, time::Instant};
pub fn main(args: &[String]) -> Result<()> {
    let request: Value = serde_json::from_reader(File::open(
        args.get(2).context("benchmark requires fixture JSON")?,
    )?)?;
    tch::set_num_threads(2);
    tch::set_num_interop_threads(1);
    rayon::ThreadPoolBuilder::new()
        .num_threads(4)
        .build_global()?;
    let board = Board::new(2, 4, false);
    let mut net = Network::new(
        2,
        board.grid(),
        board.action_size(),
        4,
        64,
        tch::Device::Cuda(0),
    );
    net.vs
        .load(request["weights"].as_str().context("weights required")?)?;
    let mut states = vec![];
    for path in request["paths"].as_array().context("paths required")? {
        let mut s = board.initial();
        for a in path.as_array().unwrap() {
            s = board
                .legal(&s)
                .into_iter()
                .find(|(i, _)| *i == a.as_u64().unwrap() as usize)
                .context("illegal fixture action")?
                .1;
        }
        states.push(s);
    }
    anyhow::ensure!(!states.is_empty(), "empty positions");
    for batch in [1, 4, 16] {
        let work: Vec<_> = (0..batch)
            .map(|i| states[i % states.len()].clone())
            .collect();
        search::policies(&board, &work, &net, 64, &mut Random(7), false, || Ok(()))?;
        let started = Instant::now();
        let repeats = if batch == 1 { 12 } else { 3 };
        for i in 0..repeats {
            let work: Vec<_> = (0..batch)
                .map(|j| states[(i * batch + j) % states.len()].clone())
                .collect();
            search::policies(&board, &work, &net, 64, &mut Random(7), false, || Ok(()))?;
        }
        let seconds = started.elapsed().as_secs_f64();
        println!(
            "{}",
            json!({"batch":batch,"decisions":batch*repeats,"seconds":seconds,"decisions_per_second":(batch*repeats) as f64/seconds,"simulations":64,"threads":4})
        );
    }
    Ok(())
}
