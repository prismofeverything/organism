//! Scratch audit: replay self-play OGF games under the run's rules and count how
//! often each action type was available versus chosen.
use organism_train::game::{Board, Kind, Rules, State};
use serde_json::Value;
use std::{collections::BTreeMap, fs::File, io::BufReader, path::Path};
type Counts = BTreeMap<String, u64>;
fn inc(c: &mut Counts, k: impl Into<String>) {
    *c.entry(k.into()).or_default() += 1;
}
/// Can this mode actually do something (a full from->to exists)?
fn feasible(board: &Board, mode: &State, kind: Kind) -> bool {
    let n = board.spaces.len();
    if kind != Kind::Move && kind != Kind::Circulate {
        return true;
    }
    board
        .legal(mode)
        .iter()
        .any(|(_, source)| board.legal(source).iter().any(|(a, _)| *a < n))
}
fn own_pieces(s: &State, p: usize) -> usize {
    s.pieces.iter().flatten().filter(|x| x.player == p).count()
}
fn main() -> anyhow::Result<()> {
    let dir = std::env::args().nth(1).expect("games dir");
    let mut c = Counts::new();
    for f in std::fs::read_dir(Path::new(&dir))? {
        let path = f?.path();
        if path.extension().is_none_or(|e| e != "json") {
            continue;
        }
        let g: Value = serde_json::from_reader(BufReader::new(File::open(&path)?))?;
        let players = g["players"].as_array().unwrap().len();
        let rings = g["board"]["ring-colors"].as_array().unwrap().len();
        let b = Board::new(players, rings, false).with_rules(Rules {
            eat_threshold: 5,
            require_useful_action: true,
            sacrifice_yields_nothing: true,
            elide_forced: false,
        });
        let n = b.spaces.len();
        let term = g["result"]["termination"].as_str().unwrap_or("?").to_string();
        inc(&mut c, format!("games_{term}"));
        let mut state = b.initial();
        let mut turn_start: Option<(usize, usize)> = None;
        for frame in g["frames"].as_array().unwrap().iter().skip(1) {
            let action = frame["action"].as_u64().unwrap() as usize;
            let phase = b.phase(&state);
            let legal = b.legal(&state);
            if phase == "choose_action_type" {
                let mover = state.player;
                inc(&mut c, "type_decisions");
                let mut opts = vec![];
                for (a, next) in &legal {
                    let kind = match a - n - 6 {
                        0 => Kind::Eat,
                        1 => Kind::Grow,
                        2 => Kind::Move,
                        _ => continue,
                    };
                    if feasible(&b, next, kind) {
                        opts.push(kind.name());
                        inc(&mut c, format!("type_{}_available", kind.name()));
                        if *a == action {
                            inc(&mut c, format!("type_{}_chosen", kind.name()));
                        }
                    }
                }
                inc(&mut c, format!("menu[{}]", opts.join(",")));
                if opts.contains(&"move") && action == n + 8 {
                    turn_start = Some((mover, own_pieces(&state, mover)));
                }
            }
            if phase == "choose_action" {
                let kind = state.turns.last().unwrap().choice.unwrap();
                let chosen = if action == n + 13 {
                    "pass"
                } else if action == n + 9 {
                    "circulate"
                } else {
                    kind.name()
                };
                inc(&mut c, format!("slot_{}_chose_{}", kind.name(), chosen));
            }
            state = legal
                .into_iter()
                .find(|(a, _)| *a == action)
                .ok_or_else(|| anyhow::anyhow!("illegal action in {} at {}", path.display(), frame["step"]))?
                .1;
            // Once the mover's turn has handed over, score what MOVE did to them.
            if let Some((p, before)) = turn_start {
                if state.player != p || state.winner.is_some() {
                    let after = own_pieces(&state, p);
                    inc(&mut c, if after < before { "move_turn_lost_pieces" } else { "move_turn_kept_pieces" });
                    turn_start = None;
                }
            }
        }
    }
    println!("{}", serde_json::to_string_pretty(&c)?);
    Ok(())
}
