//! Read-only audit of frozen OGF games and a training snapshot; no network/GPU use.
use organism_train::game::{Board, Kind, State};
use serde::Deserialize;
use serde_json::{Value, json};
use std::{collections::BTreeMap, fs::File, io::BufReader, path::Path};
type Counts = BTreeMap<String, u64>;
fn inc(c: &mut Counts, k: impl Into<String>) {
    *c.entry(k.into()).or_default() += 1;
}
fn available(board: &Board, mode: &State, kind: Kind) -> bool {
    let n = board.spaces.len();
    let Some((_, chosen)) = board
        .legal(mode)
        .into_iter()
        .find(|(a, _)| *a == n + 6 + kind.index())
    else {
        return false;
    };
    if kind != Kind::Move && kind != Kind::Circulate {
        return true;
    }
    board
        .legal(&chosen)
        .iter()
        .any(|(_, source)| board.legal(source).iter().any(|(a, _)| *a < n))
}
#[derive(Deserialize)]
struct Sample {
    state: State,
    pi: Vec<f32>,
    value: Vec<f32>,
}
#[derive(Deserialize)]
struct Episode {
    samples: Vec<Sample>,
}
#[derive(Deserialize)]
struct Saved {
    replay: Vec<Sample>,
    episodes: Vec<Episode>,
}
struct Preferred {
    actions: usize,
    players: usize,
    action: usize,
}
impl organism_train::search::Evaluator for Preferred {
    fn evaluate(&self, _: &[f32], batch: usize) -> anyhow::Result<(Vec<f32>, Vec<f32>)> {
        let mut p = vec![0.001; self.actions * batch];
        for i in 0..batch {
            p[i * self.actions + self.action] = 0.99;
        }
        Ok((p, vec![0.; self.players * batch]))
    }
}
fn main() -> anyhow::Result<()> {
    let arg = std::env::args().nth(1).expect("frozen model directory");
    let root = Path::new(&arg);
    let mut counts = Counts::new();
    let mut budgets = Counts::new();
    let mut examples = vec![];
    let mut game_counts = vec![];
    let mut model = None;
    for f in std::fs::read_dir(root)? {
        let path = f?.path();
        if path.file_name().unwrap() == "state.json" || path.extension().is_none_or(|e| e != "json")
        {
            continue;
        }
        let g: Value = serde_json::from_reader(BufReader::new(File::open(&path)?))?;
        let players = g["players"].as_array().unwrap().len();
        let rings = g["board"]["ring-colors"].as_array().unwrap().len();
        model = Some((players, rings));
        let b = Board::new(players, rings, false);
        let n = b.spaces.len();
        let mut state = b.initial();
        let mut local = Counts::new();
        inc(&mut counts, "games");
        for frame in g["frames"].as_array().unwrap().iter().skip(1) {
            let action = frame["action"].as_u64().unwrap() as usize;
            let phase = b.phase(&state);
            let legal = b.legal(&state);
            let mut movement = None;
            if phase == "choose_action_type" {
                inc(&mut counts, "mode_decisions");
                let mut max_budget = 0;
                let mut chosen_budget = 0;
                for (a, next) in &legal {
                    let kind = match a - n - 6 {
                        0 => Kind::Eat,
                        1 => Kind::Grow,
                        2 => Kind::Move,
                        _ => unreachable!(),
                    };
                    let slots = next.turns.last().unwrap().num_actions;
                    *budgets.entry(format!("{}_slots", kind.name())).or_default() += slots as u64;
                    max_budget = max_budget.max(slots);
                    if *a == action {
                        chosen_budget = slots;
                    }
                    let feasible = available(&b, next, kind);
                    if feasible {
                        inc(&mut counts, format!("mode_{}_available", kind.name()));
                    }
                    if kind == Kind::Move {
                        movement = Some(feasible);
                    }
                    if *a == action {
                        inc(&mut counts, format!("mode_{}_chosen", kind.name()));
                        if !feasible {
                            inc(
                                &mut counts,
                                format!("mode_{}_chosen_unavailable", kind.name()),
                            );
                        }
                    }
                }
                if chosen_budget == max_budget {
                    inc(&mut counts, "mode_chose_maximum_slots");
                }
                if movement == Some(true) {
                    inc(
                        &mut counts,
                        if action == n + 8 {
                            "move_available_chosen"
                        } else {
                            "move_available_declined"
                        },
                    );
                }
            }
            if phase == "choose_action" {
                let kind = state.turns.last().unwrap().choice.unwrap();
                inc(&mut counts, format!("{}_mode_slots", kind.name()));
                let own = available(&b, &state, kind);
                let circ = available(&b, &state, Kind::Circulate);
                if own {
                    inc(
                        &mut counts,
                        format!("{}_slot_action_available", kind.name()),
                    );
                }
                let chosen = if action == n + 13 {
                    "pass"
                } else if action == n + 9 {
                    "circulate"
                } else {
                    kind.name()
                };
                inc(
                    &mut counts,
                    format!("{}_slot_chose_{}", kind.name(), chosen),
                );
                inc(&mut local, chosen);
                if chosen == "pass" {
                    if !own && !circ {
                        inc(&mut counts, "pass_no_executable_alternative");
                    }
                    inc(
                        &mut counts,
                        if legal.len() == 1 {
                            "pass_only_menu_option"
                        } else {
                            "pass_other_menu_options"
                        },
                    );
                    if own {
                        inc(&mut counts, format!("pass_with_{}_available", kind.name()));
                    }
                    if !own && circ {
                        inc(&mut counts, "pass_with_only_circulate_alternative");
                    }
                }
                if chosen == "circulate" {
                    inc(
                        &mut counts,
                        if own {
                            "circulate_with_primary_available"
                        } else {
                            "circulate_without_primary_available"
                        },
                    );
                }
                if examples.len() < 8 && kind == Kind::Move && own && chosen != "move" {
                    examples.push(json!({"game":g["name"],"step_before":frame["step"].as_u64().unwrap()-1,"chosen":chosen,"round":state.round,"board":b.snapshot(&state)}));
                }
            }
            if phase == "move_to" && action == n + 13 {
                inc(&mut counts, "move_destination_forced_pass");
            }
            state = legal
                .into_iter()
                .find(|(a, _)| *a == action)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "illegal recorded action in {} at {}",
                        path.display(),
                        frame["step"]
                    )
                })?
                .1;
        }
        game_counts.push(json!({"game":g["name"],"result":g["result"],"actions":local}));
    }
    let (players, rings) = model.unwrap();
    let b = Board::new(players, rings, false);
    let n = b.spaces.len();
    let saved: Saved =
        serde_json::from_reader(BufReader::new(File::open(root.join("state.json"))?))?;
    let mut samples = Counts::new();
    let mut mass: BTreeMap<String, f64> = BTreeMap::new();
    for (population, iter) in [
        ("replay", saved.replay.iter().collect::<Vec<_>>()),
        (
            "pending",
            saved
                .episodes
                .iter()
                .flat_map(|e| e.samples.iter())
                .collect(),
        ),
    ] {
        for s in iter {
            inc(&mut samples, format!("{population}_all_samples"));
            if !s.value.is_empty() && s.value.iter().all(|v| *v == 0.) {
                inc(&mut samples, format!("{population}_all_draw_targets"));
            }
            let phase = b.phase(&s.state);
            if phase != "choose_action" && phase != "choose_action_type" {
                continue;
            }
            let key = format!("{population}_{phase}");
            inc(&mut samples, key.clone());
            if s.value.iter().all(|v| *v == 0.) && !s.value.is_empty() {
                inc(&mut samples, format!("{key}_draw_target"));
            }
            let max = s.pi.iter().copied().fold(f32::NEG_INFINITY, f32::max);
            let winners: Vec<_> =
                s.pi.iter()
                    .enumerate()
                    .filter(|(_, p)| **p == max)
                    .map(|(i, _)| i)
                    .collect();
            if s.state.round >= 10 {
                inc(&mut samples, format!("{key}_greedy"));
                if winners.len() > 1 {
                    inc(&mut samples, format!("{key}_greedy_tie"));
                    if winners.contains(&(n + 13)) {
                        inc(&mut samples, format!("{key}_greedy_tie_pass_wins"));
                    }
                }
            }
            for (label, index) in [
                ("eat", n + 6),
                ("grow", n + 7),
                ("move", n + 8),
                ("circulate", n + 9),
                ("pass", n + 13),
            ] {
                *mass.entry(format!("{key}_{label}")).or_default() += s.pi[index] as f64;
            }
        }
    }
    let intro = b.legal(&b.initial())[0].1.clone();
    let mode = b
        .legal(&intro)
        .into_iter()
        .find(|(a, _)| *a == n + 6)
        .unwrap()
        .1;
    let preferred = Preferred {
        actions: b.action_size(),
        players,
        action: n + 6,
    };
    let pi = organism_train::search::policies(
        &b,
        &[mode],
        &preferred,
        1,
        &mut organism_train::search::Random(1),
        false,
        || Ok(()),
    )?;
    let first_probe = json!({"network_favored_action":"eat","simulations":1,"eat_visits_fraction":pi[0][n+6],"pass_visits_fraction":pi[0][n+13]});
    println!(
        "{}",
        serde_json::to_string_pretty(
            &json!({"first_simulation_probe":first_probe,"counts":counts,"budgets":budgets,"samples":samples,"policy_mass":mass,"examples":examples,"games":game_counts})
        )?
    );
    Ok(())
}
