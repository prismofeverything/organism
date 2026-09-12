//! Resumable fixed-opponent evaluation, serviced in bounded choice-sized slices.
use crate::{
    game::{Board, State},
    search::{self, Evaluator, Random, Tree},
};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::{collections::HashMap, time::Instant};

#[derive(Serialize, Deserialize)]
pub struct Session {
    pub states: Vec<State>,
    pub steps: Vec<usize>,
    pub results: Vec<Option<Value>>,
    seen: Vec<Vec<(State, u32)>>,
    trees: Vec<Option<Tree>>,
    owners: Vec<Option<bool>>,
    rngs: Vec<Random>,
    sims: usize,
    per_seat: usize,
    max_steps: usize,
    repetition: u32,
    seed: u64,
    exploration_rounds: u32,
    active_seconds: f64,
}
impl Session {
    pub fn new(
        board: &Board,
        sims: usize,
        per_seat: usize,
        max_steps: usize,
        repetition: u32,
        seed: u64,
        exploration_rounds: u32,
    ) -> Self {
        let count = board.players * per_seat;
        Self {
            states: vec![board.initial(); count],
            steps: vec![0; count],
            results: vec![None; count],
            seen: vec![vec![]; count],
            trees: vec![None; count],
            owners: vec![None; count],
            rngs: (0..count).map(|i| Random(seed + i as u64)).collect(),
            sims,
            per_seat,
            max_steps,
            repetition,
            seed,
            exploration_rounds,
            active_seconds: 0.,
        }
    }
    pub fn done(&self) -> bool {
        self.results.iter().all(Option::is_some)
    }
    pub fn tick<E: Evaluator, F: FnMut() -> Result<()>>(
        &mut self,
        board: &Board,
        candidate: &E,
        opponent: &E,
        gpu_batch: usize,
        mut control: F,
    ) -> Result<()> {
        let start = Instant::now();
        let result = (|| -> Result<()> {
            let mut seen: Vec<HashMap<State, u32>> = self
                .seen
                .iter()
                .map(|h| h.iter().cloned().collect())
                .collect();
            for i in 0..self.states.len() {
                if self.results[i].is_some() {
                    continue;
                }
                let visits = seen[i]
                    .get(&search::repetition_state(&self.states[i]))
                    .copied()
                    .unwrap_or(0);
                let reason = if self.states[i].winner.is_some() {
                    Some("win")
                } else if self.steps[i] >= self.max_steps {
                    Some("max_steps")
                } else if self.repetition > 0 && visits >= self.repetition - 1 {
                    Some("repetition")
                } else {
                    None
                };
                if let Some(reason) = reason {
                    self.results[i] = Some(
                        json!({"seat":i%board.players,"seed":self.seed+i as u64,"winner":self.states[i].winner,
                        "candidate_won":self.states[i].winner==Some(i%board.players),"termination":reason,"steps":self.steps[i]}),
                    );
                    self.trees[i] = None;
                }
            }
            let turns: Vec<_> = self.states.iter().map(|s| s.player).collect();
            for owner in [true, false] {
                control()?;
                let active: Vec<_> = (0..self.states.len())
                    .filter(|&i| {
                        self.results[i].is_none() && (turns[i] == i % board.players) == owner
                    })
                    .collect();
                if active.is_empty() {
                    continue;
                }
                let mut trees: Vec<_> = active
                    .iter()
                    .map(|&i| {
                        if self.owners[i] == Some(owner) {
                            self.trees[i].clone()
                        } else {
                            None
                        }
                        .unwrap_or_else(|| Tree::new(self.states[i].clone(), board.players))
                    })
                    .collect();
                let limits: Vec<_> = active
                    .iter()
                    .map(|&i| search::Limits {
                        seen: Some(&seen[i]),
                        steps: self.steps[i],
                        max_steps: self.max_steps,
                        repetition: self.repetition,
                    })
                    .collect();
                search::queued(
                    board,
                    &mut trees,
                    &limits,
                    if owner { candidate } else { opponent },
                    self.sims,
                    &mut Random(0),
                    false,
                    gpu_batch,
                    &mut control,
                    &mut search::Timings::default(),
                )?;
                for (&i, mut tree) in active.iter().zip(trees) {
                    let pi = tree.policy(board);
                    let sample = if self.exploration_rounds == 0 {
                        self.steps[i] < 30
                    } else {
                        self.states[i].round < self.exploration_rounds
                    };
                    let action = if sample {
                        self.rngs[i].sample(&pi)
                    } else {
                        pi.iter()
                            .enumerate()
                            .max_by(|a, b| a.1.total_cmp(b.1))
                            .unwrap()
                            .0
                    };
                    tree.advance(action)?;
                    *seen[i]
                        .entry(search::repetition_state(&self.states[i]))
                        .or_default() += 1;
                    self.seen[i] = seen[i].iter().map(|(s, n)| (s.clone(), *n)).collect();
                    self.states[i] = tree.state().clone();
                    self.steps[i] += 1;
                    self.trees[i] = Some(tree);
                    self.owners[i] = Some(owner);
                }
            }
            Ok(())
        })();
        self.active_seconds += start.elapsed().as_secs_f64();
        result
    }
    pub fn report(&self) -> Value {
        let games: Vec<_> = self.results.iter().flatten().cloned().collect();
        let wins = games.iter().filter(|g| g["candidate_won"] == true).count();
        let cutoffs = games.iter().filter(|g| g["termination"] != "win").count();
        json!({"games":games,"wins":wins,"losses":games.len()-wins-cutoffs,"cutoffs":cutoffs,
            "win_rate_all_games":wins as f64/games.len().max(1) as f64,"games_per_seat":self.per_seat,"simulations":self.sims,
            "max_steps":self.max_steps,"repetition":self.repetition,"seed":self.seed,"opening_sampling_rounds":self.exploration_rounds,
            "seconds":self.active_seconds,"timing_basis":"active evaluation service time, excludes waiting for self-play","search_version":2})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    struct Uniform(usize, usize);
    impl Evaluator for Uniform {
        fn evaluate(&self, _: &[f32], n: usize) -> Result<(Vec<f32>, Vec<f32>)> {
            Ok((vec![1.; n * self.0], vec![0.; n * self.1]))
        }
    }
    #[test]
    fn evaluation_slices_resume_without_changing_outcomes() {
        let b = Board::new(2, 3, false);
        let net = Uniform(b.action_size(), 2);
        let mut a = Session::new(&b, 4, 2, 40, 3, 17, 10);
        for _ in 0..3 {
            a.tick(&b, &net, &net, 2, || Ok(())).unwrap();
        }
        let mut restored: Session =
            serde_json::from_str(&serde_json::to_string(&a).unwrap()).unwrap();
        for session in [&mut a, &mut restored] {
            for _ in 0..50 {
                if session.done() {
                    break;
                }
                session.tick(&b, &net, &net, 2, || Ok(())).unwrap();
            }
            assert!(session.done());
        }
        assert_eq!(a.report()["games"], restored.report()["games"]);
        assert_eq!(a.states, restored.states);
    }
}
