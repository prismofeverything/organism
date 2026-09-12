use organism_train::game::Board;
use std::io::{self, BufRead};
fn main() -> anyhow::Result<()> {
    #[cfg(feature = "gpu")]
    if std::env::args().nth(1).as_deref() == Some("train") {
        return organism_train::train::main(&std::env::args().collect::<Vec<_>>());
    }
    #[cfg(feature = "gpu")]
    if std::env::args().nth(1).as_deref() == Some("benchmark") {
        return organism_train::benchmark::main(&std::env::args().collect::<Vec<_>>());
    }
    for line in io::stdin().lock().lines() {
        let request: serde_json::Value = serde_json::from_str(&line?)?;
        let players = request["players"].as_u64().unwrap_or(2) as usize;
        let rings = request["rings"].as_u64().unwrap_or(4) as usize;
        let board = Board::new(
            players,
            rings,
            request["notches"].as_bool().unwrap_or(false),
        );
        let mut state = board.initial();
        if let Some(actions) = request["actions"].as_array() {
            for action in actions {
                let action = action.as_u64().unwrap() as usize;
                state = board
                    .legal(&state)
                    .into_iter()
                    .find(|(a, _)| *a == action)
                    .ok_or_else(|| anyhow::anyhow!("illegal action {action}"))?
                    .1;
            }
        }
        let legal = board.legal(&state);
        let response = serde_json::json!({"snapshot":board.snapshot(&state),"player":state.player,"round":state.round,"phase":board.phase(&state),"legal":legal.iter().map(|(a,_)|a).collect::<Vec<_>>(),"children":legal.iter().map(|(a,s)|serde_json::json!([a,board.snapshot(s)])).collect::<Vec<_>>(),"encoding":board.encode(&state,state.player)});
        println!("{}", serde_json::to_string(&response)?);
    }
    Ok(())
}
