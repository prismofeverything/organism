//! Resumable fixed-opponent evaluation, serviced in bounded choice-sized slices.
use crate::{
    game::{Board, State},
    search::{self, Evaluator, Random, Tree},
};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::{collections::HashMap, time::Instant};

/// Wilson score interval. Honest at the handful of decisive games these panels
/// produce, where a normal approximation is not.
fn wilson(successes: usize, total: usize, z: f64) -> (f64, f64) {
    if total == 0 {
        return (0., 1.);
    }
    let n = total as f64;
    let p = successes as f64 / n;
    let divisor = 1. + z * z / n;
    let centre = p + z * z / (2. * n);
    let margin = z * ((p * (1. - p) + z * z / (4. * n)) / n).sqrt();
    (
        ((centre - margin) / divisor).max(0.),
        ((centre + margin) / divisor).min(1.),
    )
}
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
    #[serde(default)]
    pub opponent_ids: Vec<String>,
    #[serde(default)]
    pub bootstrap_horizon: bool,
    #[serde(default)]
    seats: Vec<Vec<usize>>,
    #[serde(default)]
    model_owners: Vec<Option<usize>>,
    /// Rounds of unchanged layout that end a game as `no_progress`. Zero keeps
    /// the original protocol, so recorded sessions replay unchanged.
    #[serde(default)]
    pub stall_limit: u32,
    #[serde(default)]
    layout_since_round: Vec<u32>,
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
            opponent_ids: vec![],
            bootstrap_horizon: false,
            seats: vec![],
            model_owners: vec![],
            stall_limit: 0,
            layout_since_round: vec![],
        }
    }
    pub fn mixed(&mut self, board: &Board, identities: Vec<String>) {
        self.opponent_ids = identities;
        self.seats = (0..self.states.len())
            .map(|i| {
                let candidate = i % board.players;
                let round = i / board.players;
                (0..board.players)
                    .map(|seat| {
                        if seat == candidate {
                            0
                        } else {
                            let relative = (seat + board.players - candidate) % board.players;
                            1 + (round + relative - 1) % self.opponent_ids.len()
                        }
                    })
                    .collect()
            })
            .collect();
        self.trees.fill(None);
        self.model_owners = vec![None; self.states.len()];
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
        self.tick_many(board, &[candidate, opponent], gpu_batch, &mut control)
    }
    pub fn tick_many<E: Evaluator, F: FnMut() -> Result<()>>(
        &mut self,
        board: &Board,
        models: &[&E],
        gpu_batch: usize,
        mut control: F,
    ) -> Result<()> {
        if self.seats.is_empty() {
            self.seats = (0..self.states.len())
                .map(|i| {
                    (0..board.players)
                        .map(|p| if p == i % board.players { 0 } else { 1 })
                        .collect()
                })
                .collect();
            self.model_owners = vec![None; self.states.len()];
            self.trees.fill(None);
        }
        if self.layout_since_round.len() != self.states.len() {
            // Resuming a session recorded before this rule restarts the progress
            // counter rather than judging rounds it never observed.
            self.layout_since_round = self.states.iter().map(|s| s.round).collect();
        }
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
                } else if self.stall_limit > 0
                    && self.states[i]
                        .round
                        .saturating_sub(self.layout_since_round[i])
                        >= self.stall_limit
                {
                    Some("no_progress")
                } else {
                    None
                };
                if let Some(reason) = reason {
                    self.results[i] = Some(
                        json!({"seat":i%board.players,"seed":self.seed+i as u64,"winner":self.states[i].winner,
                        "candidate_won":self.states[i].winner==Some(i%board.players),"termination":reason,"steps":self.steps[i],"seat_models":self.seats[i]}),
                    );
                    self.trees[i] = None;
                }
            }
            let turns: Vec<_> = self.states.iter().map(|s| s.player).collect();
            for owner in 0..models.len() {
                control()?;
                let active: Vec<_> = (0..self.states.len())
                    .filter(|&i| self.results[i].is_none() && self.seats[i][turns[i]] == owner)
                    .collect();
                if active.is_empty() {
                    continue;
                }
                let mut trees: Vec<_> = active
                    .iter()
                    .map(|&i| {
                        if self.model_owners[i] == Some(owner) {
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
                        bootstrap_horizon: self.bootstrap_horizon,
                    })
                    .collect();
                search::queued(
                    board,
                    &mut trees,
                    &limits,
                    models[owner],
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
                        self.rngs[i].argmax(&pi)
                    };
                    tree.advance(action)?;
                    *seen[i]
                        .entry(search::repetition_state(&self.states[i]))
                        .or_default() += 1;
                    self.seen[i] = seen[i].iter().map(|(s, n)| (s.clone(), *n)).collect();
                    let next = tree.state().clone();
                    if !crate::train::same_progress(board, &self.states[i], &next) {
                        self.layout_since_round[i] = next.round;
                    }
                    self.states[i] = next;
                    self.steps[i] += 1;
                    self.trees[i] = Some(tree);
                    self.model_owners[i] = Some(owner);
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
        let losses = games.len() - wins - cutoffs;
        let decisive = wins + losses;
        // These panels routinely cut off more games than they decide, so report
        // the interval rather than a bare ratio that reads as precise.
        let (low, high) = wilson(wins, decisive, 1.96);
        let players = (self.states.len() / self.per_seat.max(1)).max(2);
        let score = (wins as f64 + cutoffs as f64 / players as f64) / games.len().max(1) as f64;
        json!({"games":games,"wins":wins,"losses":losses,"cutoffs":cutoffs,
            "win_rate_all_games":wins as f64/games.len().max(1) as f64,"games_per_seat":self.per_seat,"simulations":self.sims,
            "decisive_games":decisive,"decisive_win_rate":if decisive>0 {Value::from(wins as f64/decisive as f64)} else {Value::Null},
            "decisive_win_rate_interval":if decisive>0 {json!([low,high])} else {Value::Null},
            "cutoff_fraction":cutoffs as f64/games.len().max(1) as f64,
            "score":score,"score_reference":1./players as f64,
            "score_basis":"wins plus an equal share of every cutoff, over all games",
            "max_steps":self.max_steps,"repetition":self.repetition,"stall_limit":self.stall_limit,"seed":self.seed,"opening_sampling_rounds":self.exploration_rounds,
            "seconds":self.active_seconds,"timing_basis":"active evaluation service time, excludes waiting for self-play","search_version":3,"cutoff_value":if self.bootstrap_horizon {"mask"} else {"draw"},"opponent_ids":self.opponent_ids,"protocol":if self.opponent_ids.len()>1 {"mixed-history"} else {"fixed-baseline"}})
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
    fn mixed_opponents_cover_both_orders_and_resume() {
        let b = Board::new(3, 4, false);
        let net = Uniform(b.action_size(), 3);
        let mut a = Session::new(&b, 4, 2, 20, 3, 17, 10);
        a.mixed(&b, vec!["baseline".into(), "historical-20".into()]);
        for seat in 0..3 {
            assert_eq!(a.seats[seat][seat], 0);
            assert_eq!(a.seats[seat + 3][seat], 0);
            for p in 0..3 {
                if p != seat {
                    assert_ne!(a.seats[seat][p], a.seats[seat + 3][p]);
                }
            }
        }
        a.tick_many(&b, &[&net, &net, &net], 2, || Ok(())).unwrap();
        let mut c: Session = serde_json::from_str(&serde_json::to_string(&a).unwrap()).unwrap();
        for s in [&mut a, &mut c] {
            while !s.done() {
                s.tick_many(&b, &[&net, &net, &net], 2, || Ok(())).unwrap();
            }
        }
        assert_eq!(a.report()["games"], c.report()["games"]);
    }
    #[test]
    fn no_progress_cutoff_bounds_games_and_stays_off_by_default() {
        let b = Board::new(2, 3, false);
        let net = Uniform(b.action_size(), 2);
        // Off by default: a recorded session replays under its original protocol.
        let mut off = Session::new(&b, 4, 1, 4000, 0, 17, 0);
        off.tick(&b, &net, &net, 2, || Ok(())).unwrap();
        assert_eq!(off.stall_limit, 0);
        assert!(off.results.iter().all(Option::is_none));
        // A limit of one ends any game whose layout did not change this round.
        let mut on = Session::new(&b, 4, 1, 4000, 0, 17, 0);
        on.stall_limit = 1;
        for _ in 0..40 {
            if on.done() {
                break;
            }
            on.tick(&b, &net, &net, 2, || Ok(())).unwrap();
        }
        assert!(on.done(), "no-progress cutoff must terminate a stalling session");
        let report = on.report();
        assert_eq!(report["stall_limit"], 1);
        assert!(report["games"].as_array().unwrap().iter().all(|g| g["steps"]
            .as_u64()
            .unwrap()
            < 4000));
        // Restarting mid-session never retroactively judges unobserved rounds.
        let mut resumed: Session =
            serde_json::from_str(&serde_json::to_string(&on).unwrap()).unwrap();
        assert_eq!(resumed.report()["games"], report["games"]);
    }
    #[test]
    fn wilson_interval_brackets_the_ratio_and_widens_when_evidence_is_thin() {
        let (low, high) = wilson(0, 0, 1.96);
        assert_eq!((low, high), (0., 1.));
        let (low, high) = wilson(6, 8, 1.96);
        assert!(low < 0.75 && 0.75 < high, "{low} {high}");
        let (wide_low, wide_high) = wilson(3, 4, 1.96);
        assert!(wide_high - wide_low > high - low, "fewer games must not read as more certain");
        let (low, high) = wilson(0, 16, 1.96);
        assert!(low == 0. && high > 0. && high < 0.3, "{low} {high}");
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
