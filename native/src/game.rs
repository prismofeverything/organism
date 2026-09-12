//! Organism base rules with compact owned state and immutable board topology.
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Kind {
    Eat,
    Grow,
    Move,
    Circulate,
    Integrity,
    Sacrifice,
    Center,
}
impl Kind {
    pub fn index(self) -> usize {
        match self {
            Self::Eat => 0,
            Self::Grow => 1,
            Self::Move => 2,
            Self::Circulate => 3,
            _ => 4,
        }
    }
    pub fn name(self) -> &'static str {
        match self {
            Self::Eat => "eat",
            Self::Grow => "grow",
            Self::Move => "move",
            Self::Circulate => "circulate",
            Self::Integrity => "integrity",
            Self::Sacrifice => "sacrifice",
            Self::Center => "center",
        }
    }
    pub fn beats(self, other: Self) -> bool {
        (self.index() + 1) % 3 == other.index()
    }
}
pub const TYPES: [Kind; 3] = [Kind::Eat, Kind::Grow, Kind::Move];
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Capture {
    pub player: usize,
    pub kind: Kind,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Piece {
    pub player: usize,
    pub kind: Kind,
    pub food: u32,
    pub organism: usize,
    pub marks: Vec<Capture>,
    pub order: u64,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Action {
    pub kind: Kind,
    pub from: Option<usize>,
    pub to: Option<usize>,
    pub element: Option<Kind>,
    pub donors: Option<BTreeMap<usize, u32>>,
    pub pass: bool,
}
impl Action {
    fn new(kind: Kind) -> Self {
        Self {
            kind,
            from: None,
            to: None,
            element: None,
            donors: None,
            pass: false,
        }
    }
    pub fn complete(&self) -> bool {
        self.pass
            || match self.kind {
                Kind::Grow => self.element.is_some() && self.donors.is_some() && self.to.is_some(),
                _ => self.from.is_some() && self.to.is_some(),
            }
    }
    pub fn field(&self) -> &'static str {
        match self.kind {
            Kind::Eat if self.to.is_none() => "to",
            Kind::Grow if self.element.is_none() => "element",
            Kind::Grow if self.donors.is_none() => "from",
            Kind::Grow => "to",
            _ if self.from.is_none() => "from",
            _ => "to",
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct OrgTurn {
    pub organism: usize,
    pub choice: Option<Kind>,
    pub num_actions: usize,
    pub actions: Vec<Action>,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct State {
    pub pieces: Vec<Option<Piece>>,
    pub food: Vec<u32>,
    pub captures: Vec<Vec<Capture>>,
    pub player: usize,
    pub round: u32,
    pub turns: Vec<OrgTurn>,
    pub introduced: bool,
    pub partial: BTreeMap<usize, u32>,
    pub winner: Option<usize>,
    pub next_order: u64,
}
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Board {
    pub players: usize,
    pub rings: usize,
    pub symmetry: usize,
    pub spaces: Vec<(usize, usize)>,
    pub adj: Vec<Vec<usize>>,
    pub homes: Vec<Vec<usize>>,
    pub capture_limit: u32,
    pub organism_limit: usize,
}
impl Board {
    pub fn new(players: usize, rings: usize, notches: bool) -> Self {
        assert!((2..=5).contains(&players));
        assert!((3..=7).contains(&rings) && (rings >= 4 || players == 2));
        let symmetry = if players < 5 { 6 } else { 5 };
        let mut spaces = vec![(0, 0)];
        for r in 1..rings {
            for n in 0..r * symmetry {
                if !(notches && r == rings - 1 && n % r == 0) {
                    spaces.push((r, n));
                }
            }
        }
        let indices: BTreeMap<_, _> = spaces.iter().enumerate().map(|(i, s)| (*s, i)).collect();
        let adj = spaces
            .iter()
            .map(|&(r, n)| {
                let mut neighbors = vec![];
                if r == 0 {
                    neighbors.extend((0..symmetry).map(|i| (1, i)));
                } else {
                    let total = r * symmetry;
                    let along = n % r;
                    let cycle = n / r;
                    neighbors.extend([(r, (n + total - 1) % total), (r, (n + 1) % total)]);
                    let inner = ((r - 1) * symmetry).max(1);
                    let base = (r - 1) * cycle;
                    if along == 0 {
                        neighbors.push((r - 1, base % inner));
                    } else {
                        neighbors.extend([
                            (r - 1, (base + along - 1) % inner),
                            (r - 1, (base + along) % inner),
                        ]);
                    }
                    if r < rings - 1 {
                        let total = (r + 1) * symmetry;
                        let base = (r + 1) * cycle;
                        if along == 0 {
                            neighbors.extend([
                                (r + 1, (base + total - 1) % total),
                                (r + 1, base % total),
                                (r + 1, (base + 1) % total),
                            ]);
                        } else {
                            neighbors.extend([
                                (r + 1, (base + along) % total),
                                (r + 1, (base + along + 1) % total),
                            ]);
                        }
                    }
                }
                neighbors
                    .iter()
                    .filter_map(|s| indices.get(s).copied())
                    .collect()
            })
            .collect();
        let total = (rings - 1) * symmetry;
        let offset = rings.saturating_sub(4).div_ceil(2) + usize::from(players == 4);
        let homes = (0..players)
            .map(|p| {
                (0..3)
                    .map(|j| {
                        indices[&(
                            rings - 1,
                            ((p * total).div_ceil(players) + j + offset) % total,
                        )]
                    })
                    .collect()
            })
            .collect();
        Self {
            players,
            rings,
            symmetry,
            spaces,
            adj,
            homes,
            capture_limit: 5,
            organism_limit: 3,
        }
    }
    pub fn initial(&self) -> State {
        State {
            pieces: vec![None; self.spaces.len()],
            food: vec![0; self.spaces.len()],
            captures: vec![vec![]; self.players],
            player: 0,
            round: 0,
            turns: vec![],
            introduced: false,
            partial: BTreeMap::new(),
            winner: None,
            next_order: 0,
        }
    }
    pub fn action_size(&self) -> usize {
        self.spaces.len() + 34
    }
    pub fn grid(&self) -> usize {
        self.spaces
            .iter()
            .map(|s| s.1 + 1)
            .max()
            .unwrap()
            .max(self.rings)
    }
    fn add(&self, s: &mut State, at: usize, player: usize, kind: Kind, food: u32, organism: usize) {
        let order = s.pieces[at].as_ref().map(|p| p.order).unwrap_or_else(|| {
            let n = s.next_order;
            s.next_order += 1;
            n
        });
        s.pieces[at] = Some(Piece {
            player,
            kind,
            food,
            organism,
            marks: vec![],
            order,
        });
    }
    fn ordered(&self, s: &State) -> Vec<usize> {
        let mut ids: Vec<_> = (0..s.pieces.len())
            .filter(|&i| s.pieces[i].is_some())
            .collect();
        ids.sort_by_key(|&i| s.pieces[i].as_ref().unwrap().order);
        ids
    }
    fn component(&self, s: &State, start: usize) -> Vec<usize> {
        let Some(piece) = &s.pieces[start] else {
            return vec![];
        };
        let player = piece.player;
        let mut seen = vec![false; s.pieces.len()];
        let mut queue = VecDeque::from([start]);
        let mut found = vec![];
        while let Some(i) = queue.pop_front() {
            if seen[i] {
                continue;
            }
            seen[i] = true;
            if s.pieces[i].as_ref().is_some_and(|p| p.player == player) {
                found.push(i);
                queue.extend(self.adj[i].iter().copied());
            }
        }
        found
    }
    fn alive(&self, s: &State, ids: &[usize]) -> bool {
        let mut kinds = 0;
        for &i in ids {
            if let Some(p) = &s.pieces[i] {
                kinds |= 1 << p.kind.index()
            }
        }
        kinds & 7 == 7
    }
    fn relabel(&self, s: &mut State) {
        let mut seen = vec![false; s.pieces.len()];
        let mut org = 0;
        for i in self.ordered(s) {
            if !seen[i] {
                for j in self.component(s, i) {
                    seen[j] = true;
                    s.pieces[j].as_mut().unwrap().organism = org;
                }
                org += 1;
            }
        }
    }
    pub fn groups(&self, s: &State, player: usize) -> BTreeMap<usize, Vec<usize>> {
        let mut out = BTreeMap::new();
        for i in self.ordered(s) {
            let p = s.pieces[i].as_ref().unwrap();
            if p.player == player {
                out.entry(p.organism).or_insert_with(Vec::new).push(i)
            }
        }
        out
    }
    fn current(&self, s: &State) -> Vec<usize> {
        let Some(t) = s.turns.last() else {
            return vec![];
        };
        self.groups(s, s.player)
            .remove(&t.organism)
            .unwrap_or_default()
    }
    fn open(&self, s: &State, i: usize) -> Vec<usize> {
        self.adj[i]
            .iter()
            .copied()
            .filter(|&j| s.pieces[j].is_none())
            .collect()
    }
    fn destinations(&self, s: &State, i: usize, grow: bool) -> Vec<usize> {
        let p = s.pieces[i].as_ref().unwrap();
        self.open(s, i)
            .into_iter()
            .filter(|&j| {
                !self.adj[j].iter().any(|&k| {
                    s.pieces[k]
                        .as_ref()
                        .is_some_and(|q| q.player != p.player && (grow || q.kind == p.kind))
                })
            })
            .collect()
    }
    fn growable(&self, s: &State, ids: &[usize]) -> BTreeSet<usize> {
        ids.iter()
            .filter(|&&i| s.pieces[i].as_ref().unwrap().kind == Kind::Grow)
            .flat_map(|&i| self.destinations(s, i, true))
            .collect()
    }
    fn can_eat(&self, s: &State, i: usize) -> bool {
        s.pieces[i].as_ref().is_some_and(|p| p.food < 111) && !self.open(s, i).is_empty()
    }
    fn can_move(&self, s: &State, i: usize) -> bool {
        let Some(p) = &s.pieces[i] else { return false };
        p.food > 0
            && (p.kind == Kind::Move
                || self.adj[i].iter().any(|&j| {
                    s.pieces[j]
                        .as_ref()
                        .is_some_and(|q| q.player == p.player && q.kind == Kind::Move)
                }))
            && self.alive(s, &self.component(s, i))
    }
    fn deconstruct(&self, s: &mut State, i: usize) {
        if let Some(p) = s.pieces[i].take() {
            s.food[i] += p.food + 1;
        }
    }
    pub fn victory(&self, s: &State) -> Option<usize> {
        let leader = |scores: Vec<(usize, i64)>| {
            let best = scores.iter().map(|x| x.1).max()?;
            let leaders: Vec<_> = scores
                .into_iter()
                .filter(|x| x.1 == best)
                .map(|x| x.0)
                .collect();
            if leaders.len() == 1 {
                return Some(leaders[0]);
            }
            let rest: Vec<_> = leaders.into_iter().filter(|&p| p != s.player).collect();
            if rest.len() == 1 { Some(rest[0]) } else { None }
        };
        let alive = (0..self.players)
            .filter_map(|p| {
                let n = self
                    .groups(s, p)
                    .values()
                    .filter(|ids| self.alive(s, ids))
                    .count();
                (n >= self.organism_limit).then_some((p, n as i64))
            })
            .collect();
        leader(alive).or_else(|| {
            leader(
                (0..self.players)
                    .filter_map(|p| {
                        let n = s.captures[p].len() as i64 - self.capture_limit as i64;
                        (n >= 0).then_some((p, n))
                    })
                    .collect(),
            )
        })
    }
    fn conflict(&self, s: &mut State, rise: usize, fall: usize) {
        let (Some(a), Some(b)) = (s.pieces[rise].clone(), s.pieces[fall].clone()) else {
            return;
        };
        if a.kind == b.kind {
            self.deconstruct(s, rise);
            self.deconstruct(s, fall);
            s.captures[b.player].push(Capture {
                player: a.player,
                kind: a.kind,
            });
        } else {
            self.deconstruct(s, fall);
            let cap = Capture {
                player: b.player,
                kind: b.kind,
            };
            s.pieces[rise].as_mut().unwrap().marks.push(cap.clone());
            s.captures[a.player].push(cap);
        }
    }
    fn resolve(&self, s: &mut State) {
        for p in s.pieces.iter_mut().flatten() {
            p.marks.clear()
        }
        let mut conflicts = vec![];
        for i in self.ordered(s) {
            let p = s.pieces[i].as_ref().unwrap();
            if p.player == s.player {
                for &j in &self.adj[i] {
                    if let Some(q) = &s.pieces[j] {
                        if q.player != p.player {
                            conflicts.push(if p.kind.beats(q.kind) { (i, j) } else { (j, i) });
                        }
                    }
                }
            }
        }
        let mut edges: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
        let mut captor = BTreeMap::new();
        let mut annihilate = vec![];
        for (a, b) in conflicts {
            if s.pieces[a].as_ref().unwrap().kind == s.pieces[b].as_ref().unwrap().kind {
                annihilate.push((a, b))
            } else {
                edges.entry(a).or_default().insert(b);
                edges.entry(b).or_default();
                captor.insert(b, a);
            }
        }
        for (a, b) in annihilate {
            self.conflict(s, a, b)
        }
        let mut incoming: BTreeMap<usize, usize> = edges.keys().map(|&i| (i, 0)).collect();
        for targets in edges.values() {
            for t in targets {
                *incoming.get_mut(t).unwrap() += 1
            }
        }
        let mut ready: Vec<_> = incoming
            .iter()
            .filter(|(_, n)| **n == 0)
            .map(|(&i, _)| i)
            .collect();
        let mut order = vec![];
        while let Some(i) = ready.pop() {
            order.push(i);
            for &j in &edges[&i] {
                let n = incoming.get_mut(&j).unwrap();
                *n -= 1;
                if *n == 0 {
                    ready.push(j)
                }
            }
        }
        if order.len() == edges.len() {
            for b in order.into_iter().rev() {
                if let Some(&a) = captor.get(&b) {
                    self.conflict(s, a, b)
                }
            }
        }
        self.relabel(s);
        let mut lost = BTreeSet::new();
        let mut all = BTreeMap::new();
        for i in self.ordered(s) {
            all.entry(s.pieces[i].as_ref().unwrap().organism)
                .or_insert_with(Vec::new)
                .push(i);
        }
        for ids in all.values() {
            if self.alive(s, ids) {
                continue;
            }
            let owner = s.pieces[ids[0]].as_ref().unwrap().player;
            if owner == s.player {
                let victims: BTreeSet<_> = ids
                    .iter()
                    .flat_map(|&i| s.pieces[i].as_ref().unwrap().marks.iter().map(|c| c.player))
                    .collect();
                for p in victims {
                    s.captures[p].push(Capture {
                        player: owner,
                        kind: Kind::Sacrifice,
                    });
                }
            } else {
                lost.insert(owner);
            }
            for &i in ids {
                self.deconstruct(s, i)
            }
        }
        for p in lost {
            s.captures[s.player].push(Capture {
                player: p,
                kind: Kind::Integrity,
            });
        }
    }
    fn normalize(&self, s: &mut State) {
        for _ in 0..100 {
            if s.winner.is_some() {
                return;
            }
            // Resolve integrity before testing a victory caused by final actions.
            let groups = self.groups(s, s.player);
            let done = s.turns.last().is_some_and(|t| {
                t.choice.is_some()
                    && t.actions.iter().all(Action::complete)
                    && t.actions.len() >= t.num_actions
                    && s.turns.len() >= groups.len()
            });
            if done {
                self.resolve(s);
                s.winner = self.victory(s);
                if s.winner.is_some() {
                    return;
                }
                s.player = (s.player + 1) % self.players;
                if s.player == 0 {
                    s.round += 1;
                }
                s.turns.clear();
                s.introduced = false;
                if s.pieces[0].as_ref().is_some_and(|p| p.player == s.player) {
                    s.captures[s.player].push(Capture {
                        player: s.player,
                        kind: Kind::Center,
                    });
                }
                continue;
            }
            s.winner = self.victory(s);
            return;
        }
        panic!("automatic transition limit exceeded")
    }
    fn execute(&self, s: &mut State) {
        let a = s.turns.last().unwrap().actions.last().unwrap().clone();
        if !a.complete() || a.pass {
            return;
        }
        let to = a.to.unwrap();
        match a.kind {
            Kind::Eat => {
                let from = a.from.unwrap();
                s.pieces[to].as_mut().unwrap().food += 1 + s.food[from];
                s.food[from] = 0;
            }
            Kind::Grow => {
                for (i, n) in a.donors.unwrap() {
                    s.pieces[i].as_mut().unwrap().food -= n;
                }
                let food = s.food[to];
                s.food[to] = 0;
                self.add(
                    s,
                    to,
                    s.player,
                    a.element.unwrap(),
                    food,
                    s.turns.last().unwrap().organism,
                );
            }
            Kind::Move => {
                let mut p = s.pieces[a.from.unwrap()].take().unwrap();
                p.food += s.food[to];
                s.food[to] = 0;
                p.order = s.next_order;
                s.next_order += 1;
                s.pieces[to] = Some(p);
            }
            Kind::Circulate => {
                let from = a.from.unwrap();
                let amount = s.pieces[from].as_ref().unwrap().food.div_ceil(2);
                s.pieces[from].as_mut().unwrap().food -= amount;
                s.pieces[to].as_mut().unwrap().food += amount;
            }
            _ => unreachable!(),
        }
    }
    pub fn phase(&self, s: &State) -> String {
        if s.winner.is_some() {
            return "game_over".into();
        }
        let groups = self.groups(s, s.player);
        if groups.is_empty() {
            return "introduce".into();
        }
        let Some(t) = s.turns.last() else {
            return if groups.len() > 1 {
                "choose_organism"
            } else {
                "choose_action_type"
            }
            .into();
        };
        if t.choice.is_none() {
            return "choose_action_type".into();
        }
        if t.actions.iter().all(Action::complete) {
            return if t.actions.len() < t.num_actions {
                "choose_action"
            } else {
                "choose_organism"
            }
            .into();
        }
        let a = t.actions.last().unwrap();
        format!("{}_{}", a.kind.name(), a.field())
    }
    pub fn legal(&self, state: &State) -> Vec<(usize, State)> {
        let mut s = state.clone();
        self.normalize(&mut s);
        if s.winner.is_some() {
            return vec![];
        }
        let n = self.spaces.len();
        let mut out = vec![];
        let mut groups = self.groups(&s, s.player);
        if groups.is_empty() {
            let perms = [
                [0, 1, 2],
                [0, 2, 1],
                [1, 0, 2],
                [1, 2, 0],
                [2, 0, 1],
                [2, 1, 0],
            ];
            for (k, perm) in perms.iter().enumerate() {
                let mut next = s.clone();
                for &home in &self.homes[s.player] {
                    for &i in &self.adj[home] {
                        next.pieces[i] = None;
                    }
                }
                for (j, &i) in self.homes[s.player].iter().enumerate() {
                    next.food[i] = 0;
                    self.add(&mut next, i, s.player, TYPES[perm[j]], 1, 0);
                }
                next.introduced = true;
                out.push((n + k, next));
            }
        } else {
            if s.turns.is_empty() {
                self.relabel(&mut s);
                groups = self.groups(&s, s.player);
                if groups.len() == 1 {
                    s.turns.push(OrgTurn {
                        organism: *groups.keys().next().unwrap(),
                        choice: None,
                        num_actions: 0,
                        actions: vec![],
                    });
                }
            }
            let choose_org = s.turns.is_empty()
                || s.turns.last().is_some_and(|t| {
                    t.choice.is_some()
                        && t.actions.iter().all(Action::complete)
                        && t.actions.len() >= t.num_actions
                });
            if choose_org {
                for (&org, ids) in &groups {
                    if s.turns.iter().any(|t| t.organism == org) {
                        continue;
                    }
                    let mut next = s.clone();
                    next.turns.push(OrgTurn {
                        organism: org,
                        choice: None,
                        num_actions: 0,
                        actions: vec![],
                    });
                    out.push((*ids.iter().min().unwrap(), next));
                }
            } else {
                let ids = self.current(&s);
                let t = s.turns.last().unwrap();
                let mut counts = [0usize; 3];
                for &i in &ids {
                    counts[s.pieces[i].as_ref().unwrap().kind.index()] += 1;
                }
                if t.choice.is_none() {
                    for kind in TYPES {
                        let mut next = s.clone();
                        let t = next.turns.last_mut().unwrap();
                        t.choice = Some(kind);
                        t.num_actions = counts[kind.index()];
                        out.push((n + 6 + kind.index(), next));
                    }
                } else if t.actions.iter().all(Action::complete) {
                    let kind = t.choice.unwrap();
                    let feasible = match kind {
                        Kind::Eat => ids.iter().any(|&i| {
                            s.pieces[i].as_ref().unwrap().kind == Kind::Eat && self.can_eat(&s, i)
                        }),
                        Kind::Move => ids.iter().any(|&i| self.can_move(&s, i)),
                        Kind::Grow => {
                            let food: u32 = ids
                                .iter()
                                .filter_map(|&i| {
                                    s.pieces[i]
                                        .as_ref()
                                        .filter(|p| p.kind == Kind::Grow)
                                        .map(|p| p.food)
                                })
                                .sum();
                            food >= *counts.iter().min().unwrap() as u32
                                && !self.growable(&s, &ids).is_empty()
                        }
                        _ => false,
                    };
                    let mut kinds = vec![];
                    if feasible {
                        kinds.push((n + 6 + kind.index(), kind, false));
                    }
                    if ids.iter().any(|&i| s.pieces[i].as_ref().unwrap().food > 0) {
                        kinds.push((n + 9, Kind::Circulate, false));
                    }
                    kinds.push((n + 13, Kind::Circulate, true));
                    for (index, kind, pass) in kinds {
                        let mut next = s.clone();
                        let mut a = Action::new(kind);
                        a.pass = pass;
                        next.turns.last_mut().unwrap().actions.push(a);
                        out.push((index, next));
                    }
                } else {
                    let a = t.actions.last().unwrap();
                    let field = a.field();
                    let mut indices = vec![];
                    match (a.kind, field) {
                        (Kind::Eat, "to") => indices.extend(ids.iter().copied().filter(|&i| {
                            s.pieces[i].as_ref().unwrap().kind == Kind::Eat && self.can_eat(&s, i)
                        })),
                        (Kind::Eat, "from") => {
                            let open = self.open(&s, a.to.unwrap());
                            if open.iter().any(|&i| s.food[i] > 0) {
                                indices = open
                            } else {
                                indices.extend(open.into_iter().take(1));
                            }
                        }
                        (Kind::Grow, "element") => {
                            let food: u32 = ids
                                .iter()
                                .filter_map(|&i| {
                                    s.pieces[i]
                                        .as_ref()
                                        .filter(|p| p.kind == Kind::Grow)
                                        .map(|p| p.food)
                                })
                                .sum();
                            for kind in TYPES {
                                if counts[kind.index()] as u32 <= food {
                                    indices.push(n + 10 + kind.index())
                                }
                            }
                        }
                        (Kind::Grow, "from") => {
                            if counts[a.element.unwrap().index()] == 0 {
                                indices.push(n + 14)
                            } else {
                                indices.extend(ids.iter().copied().filter(|&i| {
                                    let p = s.pieces[i].as_ref().unwrap();
                                    p.kind == Kind::Grow
                                        && p.food > *s.partial.get(&i).unwrap_or(&0)
                                }));
                            }
                        }
                        (Kind::Grow, "to") => indices.extend(self.growable(&s, &ids)),
                        (Kind::Move, "from") => {
                            indices.extend(ids.iter().copied().filter(|&i| self.can_move(&s, i)))
                        }
                        (Kind::Move, "to") => {
                            indices = self.destinations(&s, a.from.unwrap(), false)
                        }
                        (Kind::Circulate, "from") => indices.extend(
                            ids.iter()
                                .copied()
                                .filter(|&i| s.pieces[i].as_ref().unwrap().food > 0),
                        ),
                        (Kind::Circulate, "to") => {
                            indices.extend(ids.iter().copied().filter(|&i| {
                                Some(i) != a.from && s.pieces[i].as_ref().unwrap().food < 111
                            }))
                        }
                        _ => unreachable!(),
                    }
                    for index in indices {
                        let mut next = s.clone();
                        let target = next.turns.last_mut().unwrap().actions.last_mut().unwrap();
                        if a.kind == Kind::Grow && field == "element" {
                            target.element = Some(TYPES[index - n - 10]);
                        } else if a.kind == Kind::Grow && field == "from" {
                            if index == n + 14 {
                                target.donors = Some(BTreeMap::new());
                            } else {
                                *next.partial.entry(index).or_default() += 1;
                                if next.partial.values().sum::<u32>()
                                    == counts[a.element.unwrap().index()] as u32
                                {
                                    target.donors = Some(std::mem::take(&mut next.partial));
                                }
                            }
                        } else if field == "from" {
                            target.from = Some(index)
                        } else {
                            target.to = Some(index)
                        }
                        self.execute(&mut next);
                        out.push((index, next));
                    }
                    if out.is_empty() {
                        let mut next = s.clone();
                        next.turns
                            .last_mut()
                            .unwrap()
                            .actions
                            .last_mut()
                            .unwrap()
                            .pass = true;
                        out.push((n + 13, next));
                    }
                }
            }
        }
        for (_, next) in &mut out {
            self.normalize(next)
        }
        out
    }
}
impl Board {
    pub fn encode(&self, s: &State, perspective: usize) -> Vec<f32> {
        let grid = self.grid();
        let area = grid * grid;
        let channels = 6 * self.players + 22;
        let mut x = vec![0.; channels * area];
        let cell = |space: usize| {
            let (r, n) = self.spaces[space];
            r * grid + n
        };
        for (i, p) in s.pieces.iter().enumerate() {
            if let Some(p) = p {
                let relative = (p.player + self.players - perspective) % self.players;
                let at = cell(i);
                x[relative * 5 * area + at] = 1.;
                x[(relative * 5 + 1 + p.kind.index()) * area + at] = 1.;
                x[(relative * 5 + 4) * area + at] = (p.food as f64 / (10. + p.food as f64)) as f32;
                if p.player == perspective {
                    x[(5 * self.players + 1) * area + at] = 1.;
                }
            }
        }
        for (i, &food) in s.food.iter().enumerate() {
            x[5 * self.players * area + cell(i)] = (food as f64 / (5. + food as f64)) as f32;
        }
        let fill = |x: &mut Vec<f32>, channel: usize, v: f32| {
            x[channel * area..(channel + 1) * area].fill(v)
        };
        for j in 0..self.players {
            let p = (perspective + j) % self.players;
            fill(
                &mut x,
                5 * self.players + 2 + j,
                (s.captures[p].len() as f64 / self.capture_limit as f64).min(1.) as f32,
            );
        }
        let c = 6 * self.players + 2;
        if let Some(t) = s.turns.last() {
            if let Some(kind) = t.choice {
                fill(&mut x, c + kind.index(), 1.);
            }
            if let Some(a) = t.actions.last() {
                fill(&mut x, c + 4 + a.kind.index(), 1.);
                if let Some(kind) = a.element {
                    fill(&mut x, c + 8 + kind.index(), 1.);
                }
                if let Some(from) = a.from {
                    x[(c + 11) * area + cell(from)] = 1.;
                }
                if let Some(to) = a.to {
                    x[(c + 12) * area + cell(to)] = 1.;
                }
                if let Some(donors) = &a.donors {
                    for (&i, &n) in donors {
                        x[(c + 11) * area + cell(i)] = (n as f64 / 10.) as f32;
                    }
                }
                fill(
                    &mut x,
                    c + 17,
                    f32::from(a.from.is_some() || a.donors.is_some()),
                );
                fill(&mut x, c + 18, f32::from(a.to.is_some()));
            }
            for (i, p) in s.pieces.iter().enumerate() {
                if let Some(p) = p {
                    if p.player == s.player {
                        if p.organism == t.organism {
                            x[(c + 13) * area + cell(i)] = 1.;
                        }
                        if s.turns[..s.turns.len() - 1]
                            .iter()
                            .any(|t| t.organism == p.organism)
                        {
                            x[(c + 14) * area + cell(i)] = 1.;
                        }
                    }
                }
            }
            fill(&mut x, c + 15, (t.actions.len() as f64 / 10.) as f32);
            fill(
                &mut x,
                c + 16,
                if t.choice.is_none() {
                    -0.1
                } else {
                    (t.num_actions as f64 / 10.) as f32
                },
            );
        }
        for (&i, &n) in &s.partial {
            x[(c + 11) * area + cell(i)] = (n as f64 / 10.) as f32;
        }
        fill(&mut x, c + 19, f32::from(!s.turns.is_empty()));
        x
    }
    pub fn snapshot(&self, s: &State) -> serde_json::Value {
        use serde_json::json;
        let names = ["orb", "mass", "brone", "laam", "stuk"];
        let elements:Vec<_>=s.pieces.iter().enumerate().filter_map(|(i,p)|p.as_ref().map(|p|json!({"player":names[p.player],"type":p.kind.name(),"space":self.spaces[i],"food":p.food}))).collect();
        let food: Vec<_> = s
            .food
            .iter()
            .enumerate()
            .filter(|(_, n)| **n > 0)
            .map(|(i, n)| json!([self.spaces[i], n]))
            .collect();
        let captures: serde_json::Map<_, _> = (0..self.players)
            .map(|p| {
                let mut caps = s.captures[p].clone();
                caps.sort_by_key(|c| (names[c.player], c.kind.name()));
                (
                    names[p].to_string(),
                    json!(
                        caps.iter()
                            .map(|c| json!({"player":names[c.player],"type":c.kind.name()}))
                            .collect::<Vec<_>>()
                    ),
                )
            })
            .collect();
        json!({"elements":elements,"food":food,"captures":captures,"winner":s.winner.map(|p|names[p])})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn compact_two_player_board_preserves_starting_clearance() {
        let b = Board::new(2, 3, false);
        assert_eq!(b.spaces.len(), 19);
        assert_eq!(b.grid(), 12);
        assert_eq!(
            b.homes[0].iter().map(|&i| b.spaces[i]).collect::<Vec<_>>(),
            vec![(2, 0), (2, 1), (2, 2)]
        );
        assert_eq!(
            b.homes[1].iter().map(|&i| b.spaces[i]).collect::<Vec<_>>(),
            vec![(2, 6), (2, 7), (2, 8)]
        );
        for (p, homes) in b.homes.iter().enumerate() {
            for &h in homes {
                assert!(b.adj[h].iter().all(|i| !b.homes[1 - p].contains(i)));
            }
        }
    }
    #[test]
    fn introduction_clears_three_homes_only() {
        let b = Board::new(2, 4, false);
        let mut s = b.initial();
        let adjacent = *b.adj[b.homes[0][0]]
            .iter()
            .find(|i| !b.homes[0].contains(i))
            .unwrap();
        for &i in &b.homes[0] {
            s.food[i] = 9;
        }
        s.food[adjacent] = 6;
        b.add(&mut s, adjacent, 1, Kind::Eat, 4, 0);
        let children = b.legal(&s);
        assert_eq!(children.len(), 6);
        for (_, next) in children {
            assert_eq!(next.food[adjacent], 6);
            assert!(next.pieces[adjacent].is_none());
            for &i in &b.homes[0] {
                assert_eq!(next.food[i], 0);
                assert_eq!(next.pieces[i].as_ref().unwrap().food, 1);
            }
        }
        assert_eq!(s.food[b.homes[0][0]], 9);
    }
    #[test]
    fn circulation_half_rounded_up() {
        let b = Board::new(2, 4, false);
        for amount in [0, 1, 2, 3, 5, 10, 111] {
            let mut s = b.initial();
            b.add(&mut s, 1, 0, Kind::Eat, amount, 0);
            b.add(&mut s, 2, 0, Kind::Move, 4, 0);
            let mut a = Action::new(Kind::Circulate);
            a.from = Some(1);
            a.to = Some(2);
            s.turns.push(OrgTurn {
                organism: 0,
                choice: Some(Kind::Circulate),
                num_actions: 1,
                actions: vec![a],
            });
            b.execute(&mut s);
            assert_eq!(s.pieces[1].as_ref().unwrap().food, amount / 2);
            assert_eq!(s.pieces[2].as_ref().unwrap().food, 4 + amount.div_ceil(2));
        }
    }
    #[test]
    fn acting_player_loses_capture_tie() {
        let b = Board::new(3, 4, false);
        let mut s = b.initial();
        let cap = Capture {
            player: 0,
            kind: Kind::Eat,
        };
        s.captures[0] = vec![cap.clone(); 5];
        s.captures[1] = vec![cap.clone(); 5];
        assert_eq!(b.victory(&s), Some(1));
        s.captures[2] = vec![cap.clone(); 5];
        assert_eq!(b.victory(&s), None);
        s.captures[2].push(cap);
        assert_eq!(b.victory(&s), Some(2));
    }
}
