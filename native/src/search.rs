//! Batched multi-player MCTS: Rust workers prepare leaves, one GPU call evaluates the batch.
use crate::game::{Board, State};
use anyhow::Result;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
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
struct Node {
    state: State,
    prior: f32,
    visits: u32,
    values: Vec<f32>,
    children: Vec<(usize, usize)>,
    expanded: bool,
}
impl Node {
    fn new(state: State, prior: f32, players: usize) -> Self {
        Self {
            state,
            prior,
            visits: 0,
            values: vec![0.; players],
            children: vec![],
            expanded: false,
        }
    }
}
struct Tree {
    nodes: Vec<Node>,
}
struct Request {
    path: Vec<usize>,
    legal: Vec<(usize, State)>,
    input: Vec<f32>,
}
impl Tree {
    fn new(s: State, players: usize) -> Self {
        Self {
            nodes: vec![Node::new(s, 1., players)],
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
        let mut path = vec![0];
        let mut i = 0;
        if !root {
            while self.nodes[i].expanded && !self.nodes[i].children.is_empty() {
                let parent = &self.nodes[i];
                let actor = parent.state.player;
                i = parent
                    .children
                    .iter()
                    .max_by(|(_, a), (_, b)| {
                        let score = |n: &Node| {
                            let q = if n.visits > 0 {
                                n.values[actor] / n.visits as f32
                            } else {
                                0.
                            };
                            q + n.prior * (parent.visits as f32).sqrt() / (1. + n.visits as f32)
                        };
                        score(&self.nodes[*a]).total_cmp(&score(&self.nodes[*b]))
                    })
                    .unwrap()
                    .1;
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
        })
    }
    fn finish(&mut self, board: &Board, r: Request, priors: &[f32], values: &[f32], root: bool) {
        let i = *r.path.last().unwrap();
        let actor = self.nodes[i].state.player;
        let mut v = vec![0.; board.players];
        for (j, &value) in values.iter().enumerate() {
            v[(actor + j) % board.players] = value;
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
            self.nodes[*i].prior = 0.75 * self.nodes[*i].prior + 0.25 * (eta / sum) as f32;
        }
    }
}
pub fn policies<E: Evaluator, F: FnMut() -> Result<()>>(
    board: &Board,
    states: &[State],
    net: &E,
    sims: usize,
    rng: &mut Random,
    noise: bool,
    mut control: F,
) -> Result<Vec<Vec<f32>>> {
    let mut trees: Vec<_> = states
        .iter()
        .cloned()
        .map(|s| Tree::new(s, board.players))
        .collect();
    for iteration in 0..=sims {
        control()?;
        let root = iteration == 0;
        let requests: Vec<_> = trees
            .par_iter_mut()
            .map(|t| t.prepare(board, root))
            .collect();
        let mut inputs = vec![];
        let mut count = 0;
        for r in requests.iter().flatten() {
            inputs.extend_from_slice(&r.input);
            count += 1;
        }
        if count > 0 {
            let (priors, values) = net.evaluate(&inputs, count)?;
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
        }
        if root && noise {
            for tree in &mut trees {
                tree.noise(rng)
            }
        }
    }
    Ok(trees
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
        .collect())
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
}
