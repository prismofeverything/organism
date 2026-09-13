//! Batched multi-player MCTS: Rust workers prepare leaves, one GPU call evaluates the batch.
use crate::game::{Board, State};
use anyhow::Result;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    sync::mpsc,
    time::{Duration, Instant},
};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Random(pub u64);
impl Random {
    pub fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        z ^ (z >> 31)
    }
    pub fn unit(&mut self) -> f64 {
        ((self.next() >> 11) as f64 + 0.5) / (1u64 << 53) as f64
    }
    pub fn index(&mut self, n: usize) -> usize {
        assert!(n > 0);
        let limit = u64::MAX - u64::MAX % (n as u64);
        loop {
            let x = self.next();
            if x < limit {
                return (x % n as u64) as usize;
            }
        }
    }
    pub fn sample(&mut self, weights: &[f32]) -> usize {
        let sum: f64 = weights.iter().map(|x| *x as f64).sum();
        let mut u = self.unit() * sum;
        for (i, &w) in weights.iter().enumerate() {
            u -= w as f64;
            if u <= 0. {
                return i;
            }
        }
        weights.len() - 1
    }
    pub fn argmax(&mut self, values: &[f32]) -> usize {
        let best = values.iter().copied().max_by(f32::total_cmp).unwrap();
        let ties: Vec<_> = values
            .iter()
            .enumerate()
            .filter(|(_, v)| **v == best)
            .map(|(i, _)| i)
            .collect();
        ties[self.index(ties.len())]
    }
    fn normal(&mut self) -> f64 {
        (-2. * self.unit().ln()).sqrt() * (std::f64::consts::TAU * self.unit()).cos()
    }
    fn gamma(&mut self, a: f64) -> f64 {
        if a < 1. {
            return self.gamma(a + 1.) * self.unit().powf(1. / a);
        }
        let d = a - 1. / 3.;
        let c = (9. * d).sqrt().recip();
        loop {
            let x = self.normal();
            let v = 1. + c * x;
            if v <= 0. {
                continue;
            }
            let v = v * v * v;
            let u = self.unit();
            if u < 1. - 0.0331 * x.powi(4) || u.ln() < 0.5 * x * x + d * (1. - v + v.ln()) {
                return d * v;
            }
        }
    }
}
pub trait Evaluator {
    fn evaluate(&self, inputs: &[f32], batch: usize) -> Result<(Vec<f32>, Vec<f32>)>;
}
#[cfg(feature = "gpu")]
impl Evaluator for crate::network::Network {
    fn evaluate(&self, inputs: &[f32], batch: usize) -> Result<(Vec<f32>, Vec<f32>)> {
        self.infer(inputs, batch)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct Node {
    state: State,
    prior: f32,
    base_prior: f32,
    visits: u32,
    values: Vec<f32>,
    children: Vec<(usize, usize)>,
    expanded: bool,
    #[serde(default)]
    horizon_value: Option<Vec<f32>>,
}
impl Node {
    fn new(state: State, prior: f32, players: usize) -> Self {
        Self {
            state,
            prior,
            base_prior: prior,
            visits: 0,
            values: vec![0.; players],
            children: vec![],
            expanded: false,
            horizon_value: None,
        }
    }
}
#[derive(Clone, Serialize, Deserialize)]
pub struct Tree {
    nodes: Vec<Node>,
    #[serde(default)]
    tie_rng: Random,
}
struct Request {
    path: Vec<usize>,
    legal: Vec<(usize, State)>,
    input: Vec<f32>,
    horizon: bool,
}
impl Tree {
    pub fn new(s: State, players: usize) -> Self {
        Self {
            nodes: vec![Node::new(s, 1., players)],
            tie_rng: Random::default(),
        }
    }
    fn backup(&mut self, path: &[usize], v: &[f32]) {
        for &i in path {
            self.nodes[i].visits += 1;
            for (a, b) in self.nodes[i].values.iter_mut().zip(v) {
                *a += b
            }
        }
    }
    fn prepare(&mut self, board: &Board, root: bool) -> Option<Request> {
        self.prepare_limited(board, root, &Limits::default())
    }
    fn prepare_limited(&mut self, board: &Board, root: bool, limits: &Limits) -> Option<Request> {
        let mut path = vec![0];
        let mut i = 0;
        if !root {
            while self.nodes[i].expanded && !self.nodes[i].children.is_empty() {
                if limits.cutoff(&self.nodes, &path) || self.nodes[i].state.winner.is_some() {
                    break;
                }
                let parent = &self.nodes[i];
                let actor = parent.state.player;
                let mut scored: Vec<_> = parent
                    .children
                    .iter()
                    .map(|&(action, index)| {
                        let n = &self.nodes[index];
                        let q = if n.visits > 0 {
                            n.values[actor] / n.visits as f32
                        } else {
                            0.
                        };
                        (
                            action,
                            index,
                            q + n.prior * (parent.visits.max(1) as f32).sqrt()
                                / (1. + n.visits as f32),
                        )
                    })
                    .collect();
                scored.sort_by_key(|x| x.0);
                let scores: Vec<_> = scored.iter().map(|x| x.2).collect();
                i = scored[self.tie_rng.argmax(&scores)].1;
                path.push(i);
            }
        }
        let s = &self.nodes[i].state;
        if let Some(winner) = s.winner {
            let mut v = vec![-1. / (board.players - 1) as f32; board.players];
            v[winner] = 1.;
            self.backup(&path, &v);
            return None;
        }
        if limits.bootstrap_horizon && limits.horizon(&path) && !limits.repeated(&self.nodes, &path)
        {
            if let Some(v) = self.nodes[i].horizon_value.clone() {
                self.backup(&path, &v);
                return None;
            }
            return Some(Request {
                input: board.encode(s, s.player),
                path,
                legal: vec![],
                horizon: true,
            });
        }
        if limits.cutoff(&self.nodes, &path) {
            self.backup(&path, &vec![0.; board.players]);
            return None;
        }
        let legal = board.legal(s);
        if legal.is_empty() {
            self.backup(&path, &vec![0.; board.players]);
            self.nodes[i].expanded = true;
            return None;
        }
        Some(Request {
            path,
            legal,
            input: board.encode(s, s.player),
            horizon: false,
        })
    }
    fn finish(&mut self, board: &Board, r: Request, priors: &[f32], values: &[f32], root: bool) {
        let i = *r.path.last().unwrap();
        let actor = self.nodes[i].state.player;
        let mut v = vec![0.; board.players];
        for (j, &value) in values.iter().enumerate() {
            v[(actor + j) % board.players] = value;
        }
        if r.horizon {
            self.nodes[i].horizon_value = Some(v.clone());
            self.backup(&r.path, &v);
            return;
        }
        let sum: f32 = r.legal.iter().map(|(a, _)| priors[*a]).sum();
        let count = r.legal.len();
        for (a, s) in r.legal {
            let child = self.nodes.len();
            self.nodes.push(Node::new(
                s,
                if sum > 0. {
                    priors[a] / sum
                } else {
                    1. / count as f32
                },
                board.players,
            ));
            self.nodes[i].children.push((a, child));
        }
        self.nodes[i].expanded = true;
        if !root {
            self.backup(&r.path, &v)
        }
    }
    fn noise(&mut self, rng: &mut Random) {
        let children = self.nodes[0].children.clone();
        let noise: Vec<_> = children.iter().map(|_| rng.gamma(0.3)).collect();
        let sum: f64 = noise.iter().sum();
        for ((_, i), eta) in children.iter().zip(noise) {
            self.nodes[*i].prior = 0.75 * self.nodes[*i].base_prior + 0.25 * (eta / sum) as f32;
        }
    }
}
/// Past real positions exclude the current decision, exactly like the game runner.
#[derive(Default, Clone)]
pub struct Limits<'a> {
    pub seen: Option<&'a HashMap<State, u32>>,
    pub steps: usize,
    pub max_steps: usize,
    pub repetition: u32,
    pub bootstrap_horizon: bool,
}
pub fn repetition_state(s: &State) -> State {
    let mut s = s.clone();
    s.round = 0;
    s.next_order = 0;
    for p in s.pieces.iter_mut().flatten() {
        p.order = 0;
    }
    s
}
impl Limits<'_> {
    fn cutoff(&self, nodes: &[Node], path: &[usize]) -> bool {
        self.horizon(path) || self.repeated(nodes, path)
    }
    fn horizon(&self, path: &[usize]) -> bool {
        self.max_steps > 0 && self.steps + path.len() - 1 >= self.max_steps
    }
    fn repeated(&self, nodes: &[Node], path: &[usize]) -> bool {
        if self.repetition == 0 {
            return false;
        }
        let key = repetition_state(&nodes[*path.last().unwrap()].state);
        let past = self
            .seen
            .and_then(|seen| seen.get(&key))
            .copied()
            .unwrap_or(0) as usize;
        let in_path = path[..path.len() - 1]
            .iter()
            .filter(|&&i| repetition_state(&nodes[i].state) == key)
            .count();
        past + in_path >= self.repetition.saturating_sub(1) as usize
    }
}
impl Tree {
    pub fn state(&self) -> &State {
        &self.nodes[0].state
    }
    pub fn policy(&self, board: &Board) -> Vec<f32> {
        let mut p = vec![0.; board.action_size()];
        let sum: u32 = self.nodes[0]
            .children
            .iter()
            .map(|(_, i)| self.nodes[*i].visits)
            .sum();
        for &(a, i) in &self.nodes[0].children {
            p[a] = self.nodes[i].visits as f32 / sum.max(1) as f32;
        }
        p
    }
    /// Keep only the selected subtree. No regeneration of legal successors.
    pub fn advance(&mut self, action: usize) -> Result<()> {
        let root = self.nodes[0]
            .children
            .iter()
            .find(|(a, _)| *a == action)
            .map(|(_, i)| *i)
            .ok_or_else(|| anyhow::anyhow!("search chose illegal action"))?;
        let mut order = vec![root];
        let mut map = HashMap::new();
        let mut cursor = 0;
        while cursor < order.len() {
            let old = order[cursor];
            map.insert(old, cursor);
            order.extend(self.nodes[old].children.iter().map(|(_, i)| *i));
            cursor += 1;
        }
        if order.len() > 4096 {
            *self = Self::new(
                self.nodes[root].state.clone(),
                self.nodes[root].values.len(),
            );
            return Ok(());
        }
        self.nodes = order
            .iter()
            .map(|&i| {
                let mut n = self.nodes[i].clone();
                n.children = n.children.iter().map(|&(a, i)| (a, map[&i])).collect();
                n.prior = n.base_prior;
                n
            })
            .collect();
        Ok(())
    }
}

/// Bounded pipeline: fixed cohorts preserve per-game ordering and inference batch
/// composition. CPU jobs prepare the next leaves while the owner runs another
/// cohort on its network. Exactly one request per tree is ever outstanding.
pub fn queued<E: Evaluator, F: FnMut() -> Result<()>>(
    board: &Board,
    trees: &mut [Tree],
    limits: &[Limits],
    net: &E,
    sims: usize,
    rng: &mut Random,
    noise: bool,
    batch: usize,
    mut control: F,
    timings: &mut Timings,
) -> Result<()> {
    anyhow::ensure!(
        trees.len() == limits.len() && batch > 0,
        "invalid queue layout"
    );
    let start = Instant::now();
    let seeds: Vec<_> = trees.iter().map(|_| rng.next()).collect();
    for (tree, seed) in trees.iter_mut().zip(&seeds) {
        tree.tie_rng = Random(*seed);
    }
    struct Job<'a> {
        trees: &'a mut [Tree],
        limits: &'a [Limits<'a>],
        seeds: &'a [u64],
        iteration: usize,
    }
    struct Ready<'a> {
        job: Job<'a>,
        requests: Vec<Option<Request>>,
        inputs: Vec<f32>,
        prepare: f64,
        pack: f64,
    }
    fn submit<'a>(
        scope: &rayon::Scope<'a>,
        board: &'a Board,
        job: Job<'a>,
        tx: mpsc::SyncSender<Result<Ready<'a>>>,
    ) {
        scope.spawn(move |_| {
            let ready = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let start = Instant::now();
                let requests: Vec<_> = job
                    .trees
                    .par_iter_mut()
                    .zip(job.limits)
                    .map(|(t, l)| {
                        if job.iteration == 0 && t.nodes[0].expanded {
                            None
                        } else {
                            t.prepare_limited(board, job.iteration == 0, l)
                        }
                    })
                    .collect();
                let prepare = start.elapsed().as_secs_f64();
                let start = Instant::now();
                let size = requests.iter().flatten().map(|r| r.input.len()).sum();
                let mut inputs = Vec::with_capacity(size);
                for r in requests.iter().flatten() {
                    inputs.extend_from_slice(&r.input);
                }
                let pack = start.elapsed().as_secs_f64();
                Ready {
                    job,
                    requests,
                    inputs,
                    prepare,
                    pack,
                }
            }))
            .map_err(|_| anyhow::anyhow!("search preparation worker panicked"));
            // Deliver failure too: otherwise the coordinator could wait forever.
            let _ = tx.send(ready);
        });
    }
    let result = rayon::in_place_scope(|scope| -> Result<()> {
        let count = trees.len().div_ceil(batch);
        let (tx, rx) = mpsc::sync_channel(count.max(1));
        for ((trees, limits), seeds) in trees
            .chunks_mut(batch)
            .zip(limits.chunks(batch))
            .zip(seeds.chunks(batch))
        {
            submit(
                scope,
                board,
                Job {
                    trees,
                    limits,
                    seeds,
                    iteration: 0,
                },
                tx.clone(),
            );
        }
        let mut completed = 0;
        while completed < count {
            control()?;
            let mut ready = match rx.recv_timeout(Duration::from_millis(10)) {
                Ok(r) => r?,
                Err(mpsc::RecvTimeoutError::Timeout) => continue,
                Err(e) => return Err(e.into()),
            };
            timings.prepare_seconds += ready.prepare;
            timings.pack_seconds += ready.pack;
            let n = ready.requests.iter().flatten().count();
            if n > 0 {
                let start = Instant::now();
                let (priors, values) = net.evaluate(&ready.inputs, n)?;
                timings.inference_seconds += start.elapsed().as_secs_f64();
                timings.inference_calls += 1;
                timings.evaluated_positions += n;
                let start = Instant::now();
                let mut j = 0;
                for (t, r) in ready.job.trees.iter_mut().zip(ready.requests) {
                    if let Some(r) = r {
                        t.finish(
                            board,
                            r,
                            &priors[j * board.action_size()..(j + 1) * board.action_size()],
                            &values[j * board.players..(j + 1) * board.players],
                            ready.job.iteration == 0,
                        );
                        j += 1;
                    }
                }
                timings.backup_seconds += start.elapsed().as_secs_f64();
            }
            if ready.job.iteration == 0 && noise {
                for (t, &seed) in ready.job.trees.iter_mut().zip(ready.job.seeds) {
                    t.noise(&mut Random(seed));
                }
            }
            if ready.job.iteration == sims {
                completed += 1;
            } else {
                ready.job.iteration += 1;
                submit(scope, board, ready.job, tx.clone());
            }
        }
        Ok(())
    });
    timings.total_seconds += start.elapsed().as_secs_f64();
    result
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
pub struct Timings {
    pub prepare_seconds: f64,
    pub pack_seconds: f64,
    pub inference_seconds: f64,
    pub backup_seconds: f64,
    pub total_seconds: f64,
    pub evaluated_positions: usize,
    pub inference_calls: usize,
}
pub fn policies<E: Evaluator, F: FnMut() -> Result<()>>(
    board: &Board,
    states: &[State],
    net: &E,
    sims: usize,
    rng: &mut Random,
    noise: bool,
    control: F,
) -> Result<Vec<Vec<f32>>> {
    policies_profiled(
        board,
        states,
        net,
        sims,
        rng,
        noise,
        control,
        &mut Timings::default(),
    )
}
pub fn policies_profiled<E: Evaluator, F: FnMut() -> Result<()>>(
    board: &Board,
    states: &[State],
    net: &E,
    sims: usize,
    rng: &mut Random,
    noise: bool,
    mut control: F,
    timings: &mut Timings,
) -> Result<Vec<Vec<f32>>> {
    let total = std::time::Instant::now();
    let mut trees: Vec<_> = states
        .iter()
        .cloned()
        .map(|s| Tree::new(s, board.players))
        .collect();
    // Reuse the packed input allocation across simulations; values remain identical.
    let mut inputs = Vec::new();
    for iteration in 0..=sims {
        control()?;
        let root = iteration == 0;
        let phase = std::time::Instant::now();
        let requests: Vec<_> = trees
            .par_iter_mut()
            .map(|t| t.prepare(board, root))
            .collect();
        timings.prepare_seconds += phase.elapsed().as_secs_f64();
        let phase = std::time::Instant::now();
        inputs.clear();
        let mut count = 0;
        for r in requests.iter().flatten() {
            inputs.extend_from_slice(&r.input);
            count += 1;
        }
        timings.pack_seconds += phase.elapsed().as_secs_f64();
        if count > 0 {
            let phase = std::time::Instant::now();
            let (priors, values) = net.evaluate(&inputs, count)?;
            timings.inference_seconds += phase.elapsed().as_secs_f64();
            timings.inference_calls += 1;
            timings.evaluated_positions += count;
            let phase = std::time::Instant::now();
            let mut j = 0;
            for (tree, request) in trees.iter_mut().zip(requests) {
                if let Some(r) = request {
                    tree.finish(
                        board,
                        r,
                        &priors[j * board.action_size()..(j + 1) * board.action_size()],
                        &values[j * board.players..(j + 1) * board.players],
                        root,
                    );
                    j += 1;
                }
            }
            timings.backup_seconds += phase.elapsed().as_secs_f64();
        }
        if root && noise {
            for tree in &mut trees {
                tree.noise(rng)
            }
        }
    }
    let result = trees
        .into_iter()
        .map(|tree| {
            let mut p = vec![0.; board.action_size()];
            let sum: u32 = tree.nodes[0]
                .children
                .iter()
                .map(|(_, i)| tree.nodes[*i].visits)
                .sum();
            for &(a, i) in &tree.nodes[0].children {
                p[a] = tree.nodes[i].visits as f32 / sum.max(1) as f32;
            }
            p
        })
        .collect();
    timings.total_seconds += total.elapsed().as_secs_f64();
    Ok(result)
}
#[cfg(test)]
mod tests {
    use super::*;
    struct Uniform(usize, usize);
    impl Evaluator for Uniform {
        fn evaluate(&self, _: &[f32], n: usize) -> Result<(Vec<f32>, Vec<f32>)> {
            Ok((vec![1. / self.0 as f32; n * self.0], vec![0.; n * self.1]))
        }
    }
    #[test]
    fn first_simulation_uses_prior_and_ties_ignore_child_order() {
        let b = Board::new(2, 3, false);
        let mut base = Tree::new(b.initial(), 2);
        let r = base.prepare(&b, true).unwrap();
        let favored = r.legal[0].0;
        let mut priors = vec![0.01; b.action_size()];
        priors[favored] = 0.9;
        base.finish(&b, r, &priors, &[0., 0.], true);
        let r = base.prepare(&b, false).unwrap();
        assert_eq!(
            r.path[1],
            base.nodes[0]
                .children
                .iter()
                .find(|x| x.0 == favored)
                .unwrap()
                .1
        );
        for n in base.nodes.iter_mut().skip(1) {
            n.prior = 1.;
        }
        let mut counts = HashMap::new();
        for seed in 0..1000 {
            let mut a = base.clone();
            let mut c = base.clone();
            a.tie_rng = Random(seed);
            c.tie_rng = Random(seed);
            c.nodes[0].children.reverse();
            let ar = a.prepare(&b, false).unwrap();
            let cr = c.prepare(&b, false).unwrap();
            assert_eq!(ar.path, cr.path);
            *counts.entry(ar.path[1]).or_insert(0) += 1;
        }
        assert_eq!(counts.len(), base.nodes[0].children.len());
        assert!(counts.values().all(|&n| n > 100));
    }
    #[test]
    fn horizon_bootstraps_but_repetition_still_has_neutral_value() {
        let b = Board::new(2, 3, false);
        let mut t = Tree::new(b.initial(), 2);
        let limits = Limits {
            steps: 10,
            max_steps: 10,
            bootstrap_horizon: true,
            ..Limits::default()
        };
        let r = t.prepare_limited(&b, false, &limits).unwrap();
        assert!(r.horizon && r.legal.is_empty());
        t.finish(&b, r, &[], &[0.8, -0.8], false);
        assert_eq!(t.nodes[0].values, vec![0.8, -0.8]);
        assert!(t.prepare_limited(&b, false, &limits).is_none());
        assert_eq!(t.nodes[0].values, vec![1.6, -1.6]);
        let seen = HashMap::from([(repetition_state(&b.initial()), 2)]);
        let mut t = Tree::new(b.initial(), 2);
        assert!(
            t.prepare_limited(
                &b,
                false,
                &Limits {
                    seen: Some(&seen),
                    repetition: 3,
                    ..limits
                }
            )
            .is_none()
        );
        assert_eq!(t.nodes[0].values, vec![0., 0.]);
    }
    #[test]
    fn cyclic_values_backup_without_two_player_sign_flip() {
        let b = Board::new(3, 4, false);
        let mut s = b.initial();
        s.player = 2;
        let mut tree = Tree::new(s, 3);
        let request = tree.prepare(&b, true).unwrap();
        tree.finish(
            &b,
            request,
            &vec![1.; b.action_size()],
            &[0.7, -0.2, -0.5],
            false,
        );
        assert_eq!(tree.nodes[0].values, vec![-0.2, -0.5, 0.7]);
        tree.nodes[0].state.winner = Some(1);
        tree.prepare(&b, true);
        assert_eq!(tree.nodes[0].visits, 2);
        assert!((tree.nodes[0].values[1] - 0.5).abs() < 1e-6);
    }
    #[test]
    fn legal_normalized_batched_policies() {
        for players in [2, 3] {
            let b = Board::new(players, 4, false);
            let p = policies(
                &b,
                &[b.initial(), b.initial()],
                &Uniform(b.action_size(), players),
                16,
                &mut Random(7),
                true,
                || Ok(()),
            )
            .unwrap();
            for p in p {
                assert!((p.iter().sum::<f32>() - 1.).abs() < 1e-6);
                for (i, v) in p.iter().enumerate() {
                    if *v > 0. {
                        assert!(b.legal(&b.initial()).iter().any(|(a, _)| *a == i));
                    }
                }
            }
        }
    }
    #[test]
    fn search_detects_historical_and_path_repetition_and_horizon() {
        let b = Board::new(2, 3, false);
        let state = b.initial();
        let history = HashMap::from([(repetition_state(&state), 2)]);
        let limits = Limits {
            seen: Some(&history),
            repetition: 3,
            ..Limits::default()
        };
        let mut t = Tree::new(state.clone(), 2);
        assert!(t.prepare_limited(&b, true, &limits).is_none());
        assert_eq!(t.nodes[0].visits, 1);
        assert_eq!(t.nodes[0].values, vec![0., 0.]);
        // A repeated position encountered twice along the hypothetical path.
        let mut child = state.clone();
        child.round = 10;
        t.nodes = vec![
            Node::new(state, 1., 2),
            Node::new(child.clone(), 1., 2),
            Node::new(child, 1., 2),
        ];
        t.nodes[0].expanded = true;
        t.nodes[0].children = vec![(0, 1)];
        t.nodes[1].expanded = true;
        t.nodes[1].children = vec![(0, 2)];
        assert!(
            t.prepare_limited(
                &b,
                false,
                &Limits {
                    repetition: 3,
                    ..Limits::default()
                }
            )
            .is_none()
        );
        assert_eq!(t.nodes[2].visits, 1);
        t.nodes[2].visits = 0;
        assert!(
            t.prepare_limited(
                &b,
                false,
                &Limits {
                    steps: 9,
                    max_steps: 10,
                    ..Limits::default()
                }
            )
            .is_none()
        );
        assert_eq!(t.nodes[2].visits, 0); // cutoff at depth one, before the grandchild
        // A real victory takes priority over a cutoff.
        t.nodes[0].state.winner = Some(1);
        t.prepare_limited(&b, true, &limits);
        assert_eq!(t.nodes[0].values, vec![-1., 1.]);
    }
    #[test]
    fn queue_order_is_independent_of_cohort_size_and_preserves_subtrees() {
        let b = Board::new(2, 3, false);
        let net = Uniform(b.action_size(), 2);
        let mut a = vec![Tree::new(b.initial(), 2); 4];
        let mut c = a.clone();
        let limits = vec![Limits::default(); 4];
        for (trees, batch) in [(&mut a, 1), (&mut c, 2)] {
            queued(
                &b,
                trees,
                &limits,
                &net,
                32,
                &mut Random(42),
                true,
                batch,
                || Ok(()),
                &mut Timings::default(),
            )
            .unwrap();
        }
        for (a, c) in a.iter_mut().zip(&c) {
            assert_eq!(a.policy(&b), c.policy(&b));
            let action = a.nodes[0]
                .children
                .iter()
                .max_by_key(|(_, i)| a.nodes[*i].visits)
                .unwrap()
                .0;
            let successor = b
                .legal(a.state())
                .into_iter()
                .find(|(i, _)| *i == action)
                .unwrap()
                .1;
            let visits = a.nodes[a.nodes[0]
                .children
                .iter()
                .find(|(i, _)| *i == action)
                .unwrap()
                .1]
                .visits;
            a.advance(action).unwrap();
            assert_eq!(a.state(), &successor);
            assert_eq!(a.nodes[0].visits, visits);
            assert!(
                a.nodes
                    .iter()
                    .all(|n| n.children.iter().all(|(_, i)| *i < a.nodes.len()))
            );
            let restored: Tree = serde_json::from_str(&serde_json::to_string(a).unwrap()).unwrap();
            assert_eq!(a.policy(&b), restored.policy(&b));
        }
    }
    #[test]
    fn root_noise_does_not_compound_and_queue_cancels_cleanly() {
        let b = Board::new(2, 3, false);
        let net = Uniform(b.action_size(), 2);
        let mut trees = vec![Tree::new(b.initial(), 2); 8];
        queued(
            &b,
            &mut trees,
            &vec![Limits::default(); 8],
            &net,
            8,
            &mut Random(1),
            true,
            2,
            || Ok(()),
            &mut Timings::default(),
        )
        .unwrap();
        let tree = &mut trees[0];
        tree.noise(&mut Random(44));
        let priors: Vec<_> = tree.nodes.iter().map(|n| n.prior).collect();
        tree.noise(&mut Random(44));
        assert_eq!(
            priors,
            tree.nodes.iter().map(|n| n.prior).collect::<Vec<_>>()
        );
        let mut calls = 0;
        let result = queued(
            &b,
            &mut trees,
            &vec![Limits::default(); 8],
            &net,
            64,
            &mut Random(1),
            true,
            2,
            || {
                calls += 1;
                anyhow::ensure!(calls < 3, "stop");
                Ok(())
            },
            &mut Timings::default(),
        );
        assert!(result.is_err());
    }
    #[test]
    fn worker_failure_is_reported_without_hanging_the_queue() {
        let board = Board::new(2, 3, false);
        let mut state = board.initial();
        state.food.clear();
        let mut trees = vec![Tree::new(state, 2)];
        let result = queued(
            &board,
            &mut trees,
            &[Limits::default()],
            &Uniform(board.action_size(), 2),
            2,
            &mut Random(1),
            false,
            1,
            || Ok(()),
            &mut Timings::default(),
        );
        assert!(result.unwrap_err().to_string().contains("worker panicked"));
    }
}
