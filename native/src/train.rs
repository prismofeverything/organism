//! Standalone training, durable generation checkpoints, and OGF telemetry.
use crate::{
    game::{Board, State},
    network::{Adam, Network},
    search::{self, Random},
};
use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::{
    collections::HashMap,
    fs::{self, File, OpenOptions},
    io::{BufWriter, Write},
    path::{Path, PathBuf},
    sync::atomic::{AtomicBool, Ordering},
    time::{Instant, SystemTime, UNIX_EPOCH},
};
use tch::{Device, Kind, Tensor};
static STOPPING: AtomicBool = AtomicBool::new(false);
unsafe extern "C" {
    fn organism_cuda_fraction(f: f64) -> i32;
    fn signal(sig: i32, handler: extern "C" fn(i32)) -> usize;
    fn nice(n: i32) -> i32;
    fn flock(fd: i32, operation: i32) -> i32;
}
extern "C" fn stop_signal(_: i32) {
    STOPPING.store(true, Ordering::Relaxed);
}
fn now() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64()
}
pub(crate) fn atomic_json(path: &Path, data: &impl Serialize) -> Result<()> {
    let tmp = path.with_extension("json.tmp");
    let mut file = BufWriter::with_capacity(1024 * 1024, File::create(&tmp)?);
    serde_json::to_writer(&mut file, data)?;
    file.flush()?;
    file.get_ref().sync_all()?;
    fs::rename(tmp, path)?;
    Ok(())
}
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Config {
    pub players: usize,
    pub rings: usize,
    pub blocks: usize,
    pub filters: i64,
    pub sims: usize,
    pub actors: usize,
    pub max_steps: usize,
    pub repetition: u32,
    pub replay: usize,
    pub batch: usize,
    pub train_steps: usize,
}
fn supervised() -> f32 {
    1.
}

#[derive(Clone, Serialize, Deserialize)]
struct Sample {
    state: State,
    pi: Vec<f32>,
    value: Vec<f32>,
    #[serde(default)]
    game_id: Option<String>,
    #[serde(default)]
    termination: Option<String>,
    #[serde(default = "supervised")]
    value_weight: f32,
}
#[derive(Serialize, Deserialize)]
struct Episode {
    state: State,
    samples: Vec<Sample>,
    seen: Vec<(State, u32)>,
    frames: Vec<Value>,
    result: Option<Value>,
    id: String,
    started: f64,
    #[serde(default)]
    tree: Option<search::Tree>,
    #[serde(default)]
    layout_since_round: Option<u32>,
    #[serde(default)]
    longest_layout_rounds: u32,
}
impl Episode {
    fn new(board: &Board, iteration: u64, number: usize, rng: &mut Random) -> Self {
        let state = board.initial();
        Self {
            state: state.clone(),
            samples: vec![],
            seen: vec![],
            frames: vec![frame(board, &state, 0, None)],
            result: None,
            id: format!("{iteration:06}-{number:02}-{:016x}", rng.next()),
            started: now(),
            tree: None,
            layout_since_round: Some(0),
            longest_layout_rounds: 0,
        }
    }
}
#[derive(Serialize, Deserialize)]
struct Saved {
    version: u32,
    config: Config,
    iteration: u64,
    rng: Random,
    replay: Vec<Sample>,
    replay_pos: usize,
    episodes: Vec<Episode>,
    training_step: usize,
    policy_sum: f64,
    value_sum: f64,
    elapsed: f64,
    #[serde(default)]
    last_metrics: Option<Value>,
    #[serde(default = "missing_decisions")]
    decisions: usize,
    #[serde(default)]
    search_timings: search::Timings,
    #[serde(default)]
    search_settings: Option<SearchSettings>,
}
// Fast draw-only iterations must not anneal learning away before competence.
fn masked_value_loss(v: &Tensor, target: &Tensor, weights: &Tensor) -> Tensor {
    ((v - target)
        .square()
        .mean_dim(&[-1i64][..], false, Kind::Float)
        * weights)
        .sum(Kind::Float)
        / weights.sum(Kind::Float).clamp_min(1.)
}
fn learning_rate(iteration: u64) -> f64 {
    (1e-3 * 0.5f64.powi((iteration / 20).min(4) as i32)).max(1e-4)
}
fn missing_decisions() -> usize {
    usize::MAX
}
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct SearchSettings {
    exploration_rounds: u32,
    gpu_batch: usize,
    reuse: bool,
    legacy: bool,
    #[serde(default)]
    protocol: u32,
    #[serde(default)]
    replay_game_cap: usize,
    #[serde(default)]
    mask_cutoffs: bool,
}
impl SearchSettings {
    fn sample(&self, state: &State, choices: usize) -> bool {
        if self.exploration_rounds == 0 {
            choices < 30
        } else {
            state.round < self.exploration_rounds
        }
    }
}
pub(crate) use search::repetition_state;
// Diagnostic only. Food remains part of the exact repetition key and legal state.
fn same_layout(a: &State, b: &State) -> bool {
    a.captures == b.captures
        && a.pieces.len() == b.pieces.len()
        && a.pieces.iter().zip(&b.pieces).all(|(a, b)| {
            a.as_ref().map(|p| (p.player, p.kind)) == b.as_ref().map(|p| (p.player, p.kind))
        })
}
fn frame(board: &Board, s: &State, step: usize, action: Option<usize>) -> Value {
    let names = ["orb", "mass", "brone", "laam", "stuk"];
    json!({"turn":step,"step":step,"round":s.round,"player":if s.winner.is_none(){Some(names[s.player])}else{None},"phase":board.phase(s),"action":action,"winner":s.winner.map(|p|names[p]),
        "elements":s.pieces.iter().enumerate().filter_map(|(i,p)|p.as_ref().map(|p|json!([names[p.player],p.kind.name(),format!("{}:{}",board.spaces[i].0,board.spaces[i].1),p.food]))).collect::<Vec<_>>(),
        "food":s.food.iter().enumerate().filter(|(_,n)|**n>0).map(|(i,n)|(format!("{}:{}",board.spaces[i].0,board.spaces[i].1),json!(n))).collect::<serde_json::Map<_,_>>(),
        "captures":(0..board.players).map(|p|(names[p].to_string(),json!(s.captures[p].len()))).collect::<serde_json::Map<_,_>>()})
}
fn ogf(board: &Board, e: &Episode, iteration: u64, number: usize) -> Value {
    ogf_frames(board, e, iteration, number, &e.frames)
}
fn ring_label(mut index: usize) -> String {
    let mut letters = vec![];
    loop {
        letters.push((b'A' + (index % 26) as u8) as char);
        if index < 26 {
            break;
        }
        index = index / 26 - 1;
    }
    letters.into_iter().rev().collect()
}
fn ogf_space(id: &str) -> String {
    if let Some((ring, index)) = id.split_once(':') {
        if let Ok(ring) = ring.parse::<usize>() {
            return format!("{}{index}", ring_label(ring));
        }
    }
    id.to_owned()
}
fn view_frame(frame: &Value) -> Value {
    let mut f = frame.clone();
    if let Some(elements) = f["elements"].as_array_mut() {
        for e in elements {
            e[2] = json!(ogf_space(e[2].as_str().unwrap()));
        }
    }
    if let Some(food) = f["food"].as_object() {
        f["food"] = json!(
            food.iter()
                .map(|(s, n)| (ogf_space(s), n.clone()))
                .collect::<serde_json::Map<_, _>>()
        );
    }
    f
}
fn ring_palette(count: usize) -> Vec<String> {
    let base = [
        "#fff88c", "#da6558", "#849cd5", "#febe48", "#a6cd7a", "#9c6d8e", "#3b545c",
    ];
    (0..count)
        .map(|i| {
            if i < base.len() {
                base[i].to_owned()
            } else {
                format!("hsl({},55%,65%)", i * 137 % 360)
            }
        })
        .collect()
}
fn ogf_frames(
    board: &Board,
    e: &Episode,
    iteration: u64,
    number: usize,
    frames: &[Value],
) -> Value {
    let names = ["orb", "mass", "brone", "laam", "stuk"];
    let id = |i: usize| format!("{}{}", ring_label(board.spaces[i].0), board.spaces[i].1);
    json!({"format":"organism","version":2,"profile":"view","name":format!("organism_{}p-{}",board.players,e.id),"id":e.id,"iteration":iteration,"number":number,"source":"rust-self-play","frame-unit":"decision","started":e.started,"finished":if e.result.is_some(){Some(now())}else{None},"players":&names[..board.players],"symmetry":board.symmetry,
        "board":{"center":id(0),"ring-colors":ring_palette(board.rings),"coordinates":"rings-clockwise-30deg-v1","spaces":(0..board.spaces.len()).map(id).collect::<Vec<_>>(),"adjacencies":board.adj.iter().enumerate().map(|(i,adj)|(id(i),json!(adj.iter().map(|&i|id(i)).collect::<Vec<_>>()))).collect::<serde_json::Map<_,_>>()},
        "homes":board.homes.iter().enumerate().map(|(p,spaces)|(names[p].to_string(),json!(spaces.iter().map(|&i|id(i)).collect::<Vec<_>>()))).collect::<serde_json::Map<_,_>>(),"frames":frames.iter().map(view_frame).collect::<Vec<_>>(),"result":e.result})
}
fn publish_live(
    dir: &Path,
    board: &Board,
    e: &Episode,
    iteration: u64,
    number: usize,
    stage: &str,
) -> Result<()> {
    let frames = &e.frames[e.frames.len().saturating_sub(64)..];
    let mut data = ogf_frames(board, e, iteration, number, frames);
    data["live-stage"] = json!(stage);
    data["updated"] = json!(now());
    data["live-window"] = json!(true);
    atomic_json(&dir.join("live.json"), &data)?;
    atomic_json(&dir.parent().unwrap().join("current.json"), &data)
}
struct Control {
    root: PathBuf,
    duty: f64,
    last: Instant,
}
impl Control {
    fn stopping(&self) -> bool {
        STOPPING.load(Ordering::Relaxed) || self.root.join("STOP").exists()
    }
    fn checkpoint(&mut self) -> Result<()> {
        if self.stopping() {
            bail!("stop requested")
        }
        let delay = (self.last.elapsed().as_secs_f64() * (1. / self.duty - 1.)).min(2.);
        let until = Instant::now() + std::time::Duration::from_secs_f64(delay);
        while Instant::now() < until {
            if self.stopping() {
                bail!("stop requested")
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        self.last = Instant::now();
        Ok(())
    }
}
fn save(dir: &Path, s: &Saved, net: &Network, adam: &Adam) -> Result<()> {
    let started = Instant::now();
    let generations = dir.join("snapshots");
    fs::create_dir_all(&generations)?;
    let name = format!(
        "{}-{}-{}",
        s.iteration,
        s.training_step,
        SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos()
    );
    let path = generations.join(&name);
    fs::create_dir(&path)?;
    net.vs.save(path.join("model.ot"))?;
    adam.save(&path.join("adam.ot"))?;
    let mut file = BufWriter::with_capacity(1024 * 1024, File::create(path.join("state.json"))?);
    serde_json::to_writer(&mut file, s)?;
    file.flush()?;
    file.get_ref().sync_all()?;
    File::open(path.join("model.ot"))?.sync_all()?;
    File::open(path.join("adam.ot"))?.sync_all()?;
    File::open(&path)?.sync_all()?;
    atomic_json(&dir.join("latest.json"), &json!({"generation":name}))?;
    File::open(dir)?.sync_all()?;
    atomic_json(
        &dir.join("checkpoint-timing.json"),
        &json!({"save_seconds":started.elapsed().as_secs_f64(),"state_bytes":fs::metadata(path.join("state.json"))?.len(),"updated":now()}),
    )?;
    let mut old: Vec<_> = fs::read_dir(generations)?
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_ok_and(|t| t.is_dir()))
        .collect();
    old.sort_by_key(|e| e.metadata().and_then(|m| m.modified()).ok());
    let count = old.len().saturating_sub(2);
    for e in old.into_iter().take(count) {
        fs::remove_dir_all(e.path())?;
    }
    Ok(())
}
fn load(
    dir: &Path,
    config: Config,
    net: &mut Network,
    adam: &mut Adam,
    seed: u64,
) -> Result<Saved> {
    let started = Instant::now();
    if !dir.join("latest.json").exists() {
        return Ok(Saved {
            version: 1,
            config,
            iteration: 0,
            rng: Random(seed),
            replay: vec![],
            replay_pos: 0,
            episodes: vec![],
            training_step: 0,
            policy_sum: 0.,
            value_sum: 0.,
            elapsed: 0.,
            last_metrics: None,
            decisions: 0,
            search_timings: search::Timings::default(),
            search_settings: None,
        });
    }
    let index: Value = serde_json::from_reader(std::io::BufReader::new(File::open(
        dir.join("latest.json"),
    )?))?;
    let name = index["generation"].as_str().context("invalid generation")?;
    anyhow::ensure!(
        !name.contains('/') && !name.contains(".."),
        "invalid snapshot path"
    );
    let generation = dir.join("snapshots").join(name);
    let mut saved: Saved = serde_json::from_reader(std::io::BufReader::with_capacity(
        1024 * 1024,
        File::open(generation.join("state.json"))?,
    ))?;
    if saved.decisions == usize::MAX {
        saved.decisions = saved.episodes.iter().map(|e| e.samples.len()).sum();
    }
    let old_capacity = saved.config.replay;
    saved.config.replay = config.replay;
    if old_capacity != config.replay {
        if saved.replay.len() == old_capacity {
            saved.replay.rotate_left(saved.replay_pos);
        }
        if saved.replay.len() > config.replay {
            saved.replay.drain(..saved.replay.len() - config.replay);
        }
        saved.replay_pos = saved.replay.len() % config.replay;
    }
    anyhow::ensure!(
        saved.version == 1 && saved.config == config,
        "incompatible native checkpoint configuration"
    );
    if let Some(metric) = &saved.last_metrics {
        reconcile_metric(dir, metric)?;
    }
    net.vs.load(generation.join("model.ot"))?;
    adam.load(&generation.join("adam.ot"), net.vs.device())?;
    println!(
        "Resumed {}p: {} completed iterations, {} unfinished/finished episodes, optimizer step {}",
        config.players,
        saved.iteration,
        saved.episodes.len(),
        adam.step
    );
    atomic_json(
        &dir.join("load-timing.json"),
        &json!({"load_seconds":started.elapsed().as_secs_f64(),"updated":now()}),
    )?;
    Ok(saved)
}
fn status(dir: &Path, s: &Saved, stage: &str, index: usize, extra: Value) -> Result<()> {
    let e = s.episodes.get(index);
    let mut value = json!({"stage":stage,"updated":now(),"pid":std::process::id(),"iteration":s.iteration+1,"game_number":index+1,"game_id":e.map(|e|&e.id),"started":e.map(|e|e.started),"step":e.map(|e|e.samples.len()),"actors":s.config.actors,"rings":s.config.rings,"unchanged_layout_rounds":e.map(|e|e.state.round.saturating_sub(e.layout_since_round.unwrap_or(e.state.round))),"backend":"rust-libtorch"});
    if let Some(fields) = extra.as_object() {
        for (k, v) in fields {
            value[k] = v.clone();
        }
    }
    atomic_json(&dir.join("status.json"), &value)
}
fn game_sample_indices(length: usize, cap: usize, rng: &mut Random) -> Vec<usize> {
    let count = cap.min(length);
    (0..count)
        .map(|j| {
            let lo = j * length / count;
            let hi = (j + 1) * length / count;
            lo + rng.index(hi - lo)
        })
        .collect()
}
fn finish_game(board: &Board, dir: &Path, s: &mut Saved, i: usize, reason: &str) -> Result<()> {
    let e = &mut s.episodes[i];
    let names = ["orb", "mass", "brone", "laam", "stuk"];
    let result = json!({"terminal":e.state.winner.is_some(),"steps":e.samples.len(),"winner":e.state.winner.map(|p|names[p]),"termination":reason,"longest_unchanged_layout_rounds":e.longest_layout_rounds});
    e.result = Some(result.clone());
    let settings = s.search_settings.as_ref().unwrap();
    let cap = if settings.replay_game_cap == 0 {
        e.samples.len()
    } else {
        settings.replay_game_cap.min(e.samples.len())
    };
    let selected = game_sample_indices(e.samples.len(), cap, &mut s.rng);
    for index in selected {
        let sample = &mut e.samples[index];
        sample.game_id = Some(e.id.clone());
        sample.termination = Some(reason.to_owned());
        sample.value_weight = if settings.mask_cutoffs && reason == "max_steps" {
            0.
        } else {
            1.
        };
        sample.value = (0..board.players)
            .map(|j| {
                let p = (sample.state.player + j) % board.players;
                match e.state.winner {
                    Some(w) if p == w => 1.,
                    Some(_) => -1. / (board.players - 1) as f32,
                    None => 0.,
                }
            })
            .collect();
        if s.replay.len() < s.config.replay {
            s.replay.push(sample.clone())
        } else {
            s.replay[s.replay_pos] = sample.clone();
        }
        s.replay_pos = (s.replay_pos + 1) % s.config.replay;
    }
    atomic_json(
        &dir.join("games").join(format!("{}.json", e.id)),
        &ogf(board, e, s.iteration + 1, i + 1),
    )?;
    let mut files: Vec<_> = fs::read_dir(dir.join("games"))?
        .filter_map(|x| x.ok())
        .filter(|x| x.path().extension().is_some_and(|e| e == "json"))
        .collect();
    files.sort_by_key(|x| x.file_name());
    let count = files.len().saturating_sub(100);
    for file in files.into_iter().take(count) {
        fs::remove_file(file.path())?;
    }
    println!("{}p game {}: {}", board.players, i + 1, result);
    Ok(())
}
#[derive(Serialize, Deserialize)]
struct PendingEvaluation {
    session: crate::evaluate::Session,
    iteration: u64,
    promotion_candidate: bool,
}
struct EvaluationJob {
    saved: PendingEvaluation,
    candidate: Network,
    opponent: Network,
    historical: Option<Network>,
    path: PathBuf,
}
impl EvaluationJob {
    fn network(board: &Board, config: &Config, device: Device) -> Network {
        Network::new(
            board.players,
            board.grid(),
            board.action_size(),
            config.blocks,
            config.filters,
            device,
        )
    }
    fn load(dir: &Path, board: &Board, config: &Config, device: Device) -> Result<Option<Self>> {
        let pointer = dir.join("pending-evaluation.json");
        if !pointer.exists() {
            return Ok(None);
        }
        let value: Value = serde_json::from_reader(std::io::BufReader::new(File::open(pointer)?))?;
        let iteration = value["iteration"]
            .as_u64()
            .context("invalid evaluation pointer")?;
        let path = dir.join("evaluation-jobs").join(iteration.to_string());
        let saved: PendingEvaluation = serde_json::from_reader(std::io::BufReader::new(
            File::open(path.join("state.json"))?,
        ))?;
        anyhow::ensure!(saved.iteration == iteration, "evaluation identity mismatch");
        if saved.session.opponent_ids.is_empty() {
            atomic_json(
                &path.join("interrupted-protocol-upgrade.json"),
                &json!({"reason":"search protocol upgraded to 3; partial legacy results excluded"}),
            )?;
            fs::remove_file(dir.join("pending-evaluation.json"))?;
            atomic_json(
                &dir.join("evaluation-progress.json"),
                &json!({"stage":"interrupted by search protocol upgrade", "iteration": iteration}),
            )?;
            return Ok(None);
        }
        let mut candidate = Self::network(board, config, device);
        candidate.vs.load(path.join("candidate.ot"))?;
        let mut opponent = Self::network(board, config, device);
        opponent.vs.load(path.join("opponent.ot"))?;
        let historical = if path.join("historical.ot").exists() {
            let mut n = Self::network(board, config, device);
            n.vs.load(path.join("historical.ot"))?;
            Some(n)
        } else {
            None
        };
        Ok(Some(Self {
            historical,
            saved,
            candidate,
            opponent,
            path,
        }))
    }
    fn create(
        dir: &Path,
        board: &Board,
        config: &Config,
        net: &Network,
        iteration: u64,
        per_seat: usize,
        promotion_candidate: bool,
        settings: &SearchSettings,
    ) -> Result<Self> {
        let path = dir.join("evaluation-jobs").join(iteration.to_string());
        fs::create_dir_all(&path)?;
        let mut candidate = Self::network(board, config, net.vs.device());
        candidate.vs.copy(&net.vs)?;
        let mut opponent = Self::network(board, config, net.vs.device());
        opponent.vs.load(dir.join("baseline.ot"))?;
        candidate.vs.save(path.join("candidate.ot"))?;
        opponent.vs.save(path.join("opponent.ot"))?;
        File::open(path.join("candidate.ot"))?.sync_all()?;
        File::open(path.join("opponent.ot"))?.sync_all()?;
        let mut historical = None;
        let mut historical_id = String::new();
        let archive = dir.join("opponent-archive");
        fs::create_dir_all(&archive)?;
        if fs::read_dir(&archive)?.next().is_none() {
            let mut old: Vec<_> = fs::read_dir(dir.join("evaluation-jobs"))?
                .filter_map(|e| e.ok())
                .filter(|e| e.path().join("candidate.ot").exists() && e.path() != path)
                .collect();
            old.sort_by_key(|e| e.file_name().to_string_lossy().parse::<u64>().unwrap_or(0));
            if let Some(previous) = old.last() {
                let n = previous.file_name().to_string_lossy().parse::<u64>()?;
                fs::copy(
                    previous.path().join("candidate.ot"),
                    archive.join(format!("{n:09}.ot")),
                )?;
            }
        }
        let mut entries: Vec<_> = fs::read_dir(&archive)?
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().is_some_and(|x| x == "ot"))
            .collect();
        entries.sort_by_key(|e| e.file_name());
        if !entries.is_empty() {
            let chosen = &entries[(iteration as usize / 5) % entries.len()];
            let mut n = Self::network(board, config, net.vs.device());
            n.vs.load(chosen.path())?;
            n.vs.save(path.join("historical.ot"))?;
            File::open(path.join("historical.ot"))?.sync_all()?;
            historical_id = chosen.file_name().to_string_lossy().into_owned();
            historical = Some(n);
        }
        let archive_file = archive.join(format!("{iteration:09}.ot"));
        candidate.vs.save(&archive_file)?;
        File::open(&archive_file)?.sync_all()?;
        // Preserve the oldest anchor and seven recent versions.
        for entry in entries.iter().skip(1).take(entries.len().saturating_sub(7)) {
            fs::remove_file(entry.path())?;
        }
        let mut saved = PendingEvaluation {
            session: crate::evaluate::Session::new(
                board,
                config.sims,
                if historical.is_some() {
                    per_seat.max(2).div_ceil(2) * 2
                } else {
                    per_seat
                },
                config.max_steps,
                config.repetition,
                9917,
                settings.exploration_rounds,
            ),
            iteration,
            promotion_candidate,
        };
        saved.session.bootstrap_horizon = settings.mask_cutoffs;
        saved.session.mixed(
            board,
            if historical.is_some() {
                vec!["baseline".into(), historical_id]
            } else {
                vec!["baseline".into()]
            },
        );
        let job = Self {
            historical,
            saved,
            candidate,
            opponent,
            path,
        };
        job.persist()?;
        File::open(&job.path)?.sync_all()?;
        atomic_json(
            &dir.join("pending-evaluation.json"),
            &json!({"iteration":iteration}),
        )?;
        Ok(job)
    }
    fn persist(&self) -> Result<()> {
        atomic_json(&self.path.join("state.json"), &self.saved)
    }
}
fn service_evaluation(
    job: &mut Option<EvaluationJob>,
    board: &Board,
    dir: &Path,
    settings: &SearchSettings,
    control: &mut Control,
) -> Result<()> {
    let Some(eval) = job.as_mut() else {
        return Ok(());
    };
    let mut models = vec![&eval.candidate, &eval.opponent];
    if let Some(n) = &eval.historical {
        models.push(n);
    }
    eval.saved
        .session
        .tick_many(board, &models, settings.gpu_batch, || control.checkpoint())?;
    atomic_json(
        &dir.join("evaluation-progress.json"),
        &json!({"iteration":eval.saved.iteration,"updated":now(),"completed":eval.saved.session.results.iter().filter(|r|r.is_some()).count(),"games":eval.saved.session.results.len(),"choices":eval.saved.session.steps,"stage":if eval.saved.session.done(){"complete"}else{"running alongside self-play"}}),
    )?;
    if eval.saved.session.done() {
        let mut report = eval.saved.session.report();
        report["iteration"] = json!(eval.saved.iteration);
        report["opponent"] = json!(eval.saved.session.opponent_ids);
        atomic_json(&dir.join("evaluation.json"), &report)?;
        fs::create_dir_all(dir.join("evaluations"))?;
        atomic_json(
            &dir.join("evaluations")
                .join(format!("{:06}.json", eval.saved.iteration)),
            &report,
        )?;
        if eval.saved.promotion_candidate && crate::curriculum::passes(&report) {
            let root = dir.parent().unwrap();
            let mut c: crate::curriculum::Progress = serde_json::from_reader(
                std::io::BufReader::new(File::open(root.join("curriculum.json"))?),
            )?;
            if c.rings == board.rings {
                crate::curriculum::promote(root, &mut c, &eval.candidate, &report)?;
            }
        }
        println!("{}p background evaluation: {}", board.players, report);
        fs::remove_file(dir.join("pending-evaluation.json"))?;
        // The completed candidate remains a reproducible frozen evaluation fixture.
        *job = None;
        // Bound full evaluation fixtures; lightweight reports remain in evaluations/.
        let mut fixtures: Vec<_> = fs::read_dir(dir.join("evaluation-jobs"))?
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_ok_and(|t| t.is_dir()))
            .collect();
        fixtures.sort_by_key(|e| e.file_name().to_string_lossy().parse::<u64>().unwrap_or(0));
        for entry in fixtures.iter().take(fixtures.len().saturating_sub(2)) {
            fs::remove_dir_all(entry.path())?;
        }
    }
    Ok(())
}

fn iteration(
    board: &Board,
    dir: &Path,
    s: &mut Saved,
    net: &Network,
    adam: &mut Adam,
    control: &mut Control,
    concurrent_games: usize,
    settings: &SearchSettings,
    evaluation: &mut Option<EvaluationJob>,
) -> Result<()> {
    while s.episodes.iter().filter(|e| e.result.is_some()).count() < s.config.actors
        && s.episodes.iter().filter(|e| e.result.is_none()).count() < concurrent_games
    {
        let number = s.episodes.len() + 1;
        s.episodes
            .push(Episode::new(board, s.iteration + 1, number, &mut s.rng));
    }
    let mut seen: Vec<HashMap<State, u32>> = s
        .episodes
        .iter()
        .map(|e| e.seen.iter().cloned().collect())
        .collect();
    let mut started = Instant::now();
    let mut telemetry = Instant::now() - std::time::Duration::from_secs(5);
    let mut checkpoint = Instant::now();
    let mut search_batches = 0usize;
    let result = (|| -> Result<()> {
        loop {
            control.checkpoint()?;
            for i in 0..s.episodes.len() {
                if s.episodes[i].result.is_some() {
                    continue;
                }
                let e = &s.episodes[i];
                let reason = if e.state.winner.is_some() {
                    Some("win")
                } else if e.samples.len() >= s.config.max_steps {
                    Some("max_steps")
                } else if s.config.repetition > 0
                    && seen[i]
                        .get(&repetition_state(&e.state))
                        .copied()
                        .unwrap_or(0)
                        >= s.config.repetition - 1
                {
                    Some("repetition")
                } else {
                    None
                };
                if let Some(reason) = reason {
                    finish_game(board, dir, s, i, reason)?;
                }
            }
            let finished = s.episodes.iter().filter(|e| e.result.is_some()).count();
            if finished >= s.config.actors {
                break;
            }
            while s.episodes.iter().filter(|e| e.result.is_none()).count() < concurrent_games {
                let number = s.episodes.len() + 1;
                s.episodes
                    .push(Episode::new(board, s.iteration + 1, number, &mut s.rng));
                seen.push(HashMap::new());
            }
            let active: Vec<_> = s
                .episodes
                .iter()
                .enumerate()
                .filter(|(_, e)| e.result.is_none())
                .map(|(i, _)| i)
                .collect();
            if active.is_empty() {
                break;
            }
            if telemetry.elapsed().as_secs_f64() >= 0.25 {
                let i = active[0];
                publish_live(
                    dir,
                    board,
                    &s.episodes[i],
                    s.iteration + 1,
                    i + 1,
                    "self_play",
                )?;
                status(
                    dir,
                    s,
                    "self_play",
                    i,
                    json!({"active_games":active.len(),"concurrent_games":concurrent_games,"phase":board.phase(&s.episodes[i].state)}),
                )?;
                telemetry = Instant::now();
            }
            let saved_rng = s.rng.clone();
            // Transactional search: unfinished work cannot alter a durable episode.
            let mut trees: Vec<_> = active
                .iter()
                .map(|&i| {
                    s.episodes[i].tree.clone().unwrap_or_else(|| {
                        search::Tree::new(s.episodes[i].state.clone(), board.players)
                    })
                })
                .collect();
            let policies = if settings.legacy {
                let states: Vec<_> = active
                    .iter()
                    .map(|&i| s.episodes[i].state.clone())
                    .collect();
                match search::policies_profiled(
                    board,
                    &states,
                    net,
                    s.config.sims,
                    &mut s.rng,
                    true,
                    || control.checkpoint(),
                    &mut s.search_timings,
                ) {
                    Ok(p) => p,
                    Err(e) => {
                        s.rng = saved_rng;
                        return Err(e);
                    }
                }
            } else {
                let limits: Vec<_> = active
                    .iter()
                    .map(|&i| search::Limits {
                        seen: Some(&seen[i]),
                        steps: s.episodes[i].samples.len(),
                        max_steps: s.config.max_steps,
                        repetition: s.config.repetition,
                        bootstrap_horizon: settings.mask_cutoffs,
                    })
                    .collect();
                if let Err(e) = search::queued(
                    board,
                    &mut trees,
                    &limits,
                    net,
                    s.config.sims,
                    &mut s.rng,
                    true,
                    settings.gpu_batch,
                    || control.checkpoint(),
                    &mut s.search_timings,
                ) {
                    s.rng = saved_rng;
                    return Err(e);
                }
                trees.iter().map(|t| t.policy(board)).collect()
            };
            s.decisions += active.len();
            for ((&i, pi), mut tree) in active.iter().zip(policies).zip(trees) {
                let e = &mut s.episodes[i];
                *seen[i].entry(repetition_state(&e.state)).or_default() += 1;
                let action = if settings.sample(&e.state, e.samples.len()) {
                    s.rng.sample(&pi)
                } else {
                    s.rng.argmax(&pi)
                };
                let next = if settings.legacy {
                    board
                        .legal(&e.state)
                        .into_iter()
                        .find(|(a, _)| *a == action)
                        .context("search chose illegal action")?
                        .1
                } else {
                    tree.advance(action)?;
                    let next = tree.state().clone();
                    if settings.reuse {
                        e.tree = Some(tree);
                    }
                    next
                };
                let since = e.layout_since_round.get_or_insert(e.state.round);
                e.longest_layout_rounds = e
                    .longest_layout_rounds
                    .max(next.round.saturating_sub(*since));
                if !same_layout(&e.state, &next) {
                    *since = next.round;
                }
                e.samples.push(Sample {
                    state: e.state.clone(),
                    pi,
                    value: vec![],
                    game_id: None,
                    termination: None,
                    value_weight: 1.,
                });
                e.state = next;
                e.frames
                    .push(frame(board, &e.state, e.samples.len(), Some(action)));
            }
            search_batches += 1;
            if search_batches % 8 == 0 {
                service_evaluation(evaluation, board, dir, settings, control)?;
            }
            if checkpoint.elapsed().as_secs() >= 120 {
                for (e, map) in s.episodes.iter_mut().zip(&seen) {
                    e.seen = map.iter().map(|(s, n)| (s.clone(), *n)).collect();
                }
                s.elapsed += started.elapsed().as_secs_f64();
                started = Instant::now();
                save(dir, s, net, adam)?;
                if let Some(job) = evaluation.as_ref() {
                    job.persist()?;
                }
                checkpoint = Instant::now();
            }
        }
        // Neural weights change below: invalidate every retained search tree.
        for e in &mut s.episodes {
            e.tree = None;
        }
        if !s.replay.is_empty() {
            while s.training_step < s.config.train_steps {
                control.checkpoint()?;
                status(
                    dir,
                    s,
                    "training",
                    0,
                    json!({"training_step":s.training_step,"buffer":s.replay.len()}),
                )?;
                let count = s.config.batch.min(s.replay.len());
                let mut input = vec![];
                let mut pi = vec![];
                let mut value = vec![];
                let mut weights = vec![];
                for _ in 0..count {
                    let sample = &s.replay[s.rng.index(s.replay.len())];
                    input.extend(board.encode(&sample.state, sample.state.player));
                    pi.extend_from_slice(&sample.pi);
                    value.extend_from_slice(&sample.value);
                    weights.push(sample.value_weight);
                }
                let (p, v) = net.forward(&net.input(&input, count), true);
                let target = Tensor::from_slice(&pi)
                    .view([count as i64, board.action_size() as i64])
                    .to_device(net.vs.device());
                let target_v = Tensor::from_slice(&value)
                    .view([count as i64, board.players as i64])
                    .to_device(net.vs.device());
                let pl = -(p * target)
                    .sum_dim_intlist(&[-1i64][..], false, Kind::Float)
                    .mean(Kind::Float);
                let weights = Tensor::from_slice(&weights).to_device(net.vs.device());
                let vl = masked_value_loss(&v, &target_v, &weights);
                let p_value = f64::try_from(&pl)?;
                let v_value = f64::try_from(&vl)?;
                anyhow::ensure!(p_value.is_finite() && v_value.is_finite(), "nonfinite loss");
                adam.update(net, &(&pl + &vl), learning_rate(s.iteration))?;
                s.policy_sum += p_value;
                s.value_sum += v_value;
                s.training_step += 1;
                if s.training_step % 20 == 0 {
                    service_evaluation(evaluation, board, dir, settings, control)?;
                }
            }
        }
        Ok(())
    })();
    s.elapsed += started.elapsed().as_secs_f64();
    for (e, map) in s.episodes.iter_mut().zip(seen) {
        e.seen = map.into_iter().collect();
    }
    result?;
    let stats: Vec<_> = s.episodes.iter().filter_map(|e| e.result.clone()).collect();
    let decisions = s.decisions;
    let victories = stats.iter().filter(|g| g["termination"] == "win").count();
    let seconds = s.elapsed.max(1e-9);
    let metrics = json!({"iteration":s.iteration+1,"game":format!("organism_{}p",board.players),"backend":"rust-libtorch","buffer":s.replay.len(),"replay_distinct_games":s.replay.iter().filter_map(|x|x.game_id.as_ref()).collect::<std::collections::HashSet<_>>().len(),"replay_value_supervised_fraction":s.replay.iter().map(|x|x.value_weight as f64).sum::<f64>()/s.replay.len().max(1) as f64,"policy_loss":s.policy_sum/s.training_step.max(1) as f64,"value_loss":s.value_sum/s.training_step.max(1) as f64,"total_seconds":s.elapsed,"games":stats,"optimizer_step":adam.step,"updates":s.training_step,"decisions":decisions,"games_per_hour":stats.len() as f64*3600./seconds,"rule_victories_per_hour":victories as f64*3600./seconds,"decisions_per_second":decisions as f64/seconds,"updates_per_hour":s.training_step as f64*3600./seconds,"search_timings":s.search_timings,"concurrent_games":concurrent_games,"search_settings":settings,"learning_rate":learning_rate(s.iteration)});
    atomic_json(
        &dir.join("live.json"),
        &ogf(
            board,
            s.episodes.last().unwrap(),
            s.iteration + 1,
            s.episodes.len(),
        ),
    )?;
    // Commit model/state first; reconcile the corresponding metric on load below.
    s.last_metrics = Some(metrics.clone());
    s.iteration += 1;
    s.episodes.retain(|e| e.result.is_none());
    s.decisions = 0;
    s.search_timings = search::Timings::default();
    s.training_step = 0;
    s.policy_sum = 0.;
    s.value_sum = 0.;
    s.elapsed = 0.;
    save(dir, s, net, adam)?;
    reconcile_metric(dir, &metrics)?;
    status(
        dir,
        s,
        "iteration_complete",
        0,
        json!({"iteration":s.iteration,"buffer":s.replay.len()}),
    )?;
    println!(
        "{}p completed iteration {}: {}",
        board.players, s.iteration, metrics
    );
    Ok(())
}
fn reconcile_metric(dir: &Path, metric: &Value) -> Result<()> {
    let path = dir.join("metrics.jsonl");
    let contents = fs::read_to_string(&path).unwrap_or_default();
    if contents
        .lines()
        .filter_map(|line| serde_json::from_str::<Value>(line).ok())
        .any(|row| row["iteration"] == metric["iteration"])
    {
        return Ok(());
    }
    let mut file = OpenOptions::new().create(true).append(true).open(path)?;
    // A previous process may have left a truncated final JSON line.
    if !contents.is_empty() && !contents.ends_with('\n') {
        writeln!(file)?;
    }
    writeln!(file, "{metric}")?;
    file.sync_all()?;
    Ok(())
}
fn argument(args: &[String], name: &str, default: &str) -> String {
    args.iter()
        .rposition(|a| a == name)
        .and_then(|i| args.get(i + 1))
        .cloned()
        .unwrap_or_else(|| default.into())
}
pub fn main(args: &[String]) -> Result<()> {
    let root = PathBuf::from(argument(
        args,
        "--checkpoint",
        "checkpoints/organism-native",
    ));
    fs::create_dir_all(&root)?;
    use std::os::fd::AsRawFd;
    let lock = OpenOptions::new()
        .create(true)
        .truncate(false)
        .write(true)
        .open(root.join("training.lock"))?;
    anyhow::ensure!(
        unsafe { flock(lock.as_raw_fd(), 2 | 4) } == 0,
        "another trainer owns this checkpoint root"
    );
    let duty: f64 = argument(args, "--duty", "1").parse()?;
    anyhow::ensure!(duty > 0. && duty <= 1., "duty must be in (0,1]");
    let fraction: f64 = argument(args, "--vram-fraction", "0.35").parse()?;
    anyhow::ensure!(fraction > 0. && fraction <= 1., "invalid VRAM fraction");
    let threads: usize = argument(args, "--threads", "4").parse()?;
    anyhow::ensure!(threads > 0, "threads must be positive");
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()?;
    tch::set_num_threads(2);
    tch::set_num_interop_threads(1);
    let cpu = args.iter().any(|a| a == "--cpu");
    let device = if cpu {
        Device::Cpu
    } else {
        anyhow::ensure!(tch::Cuda::is_available(), "CUDA unavailable");
        anyhow::ensure!(
            unsafe { organism_cuda_fraction(fraction) } == 0,
            "could not set GPU memory fraction"
        );
        Device::Cuda(0)
    };
    unsafe {
        signal(2, stop_signal);
        signal(15, stop_signal);
        nice(10);
    }
    let mut control = Control {
        root: root.clone(),
        duty,
        last: Instant::now(),
    };
    if control.stopping() {
        println!("STOP exists; remove it to resume");
        return Ok(());
    }
    let player_counts: Vec<usize> = argument(args, "--players", "2,3")
        .split(',')
        .map(str::parse)
        .collect::<std::result::Result<_, _>>()?;
    let cycles: usize = argument(args, "--iters", "1").parse()?;
    let forever = args.iter().any(|a| a == "--forever");
    println!(
        "Native Rust training: {device:?}, {threads} search threads, {:.0}% work duty",
        duty * 100.
    );
    let settings = SearchSettings {
        exploration_rounds: argument(args, "--exploration-rounds", "10").parse()?,
        gpu_batch: argument(args, "--gpu-batch", "16").parse()?,
        reuse: !args.iter().any(|a| a == "--no-tree-reuse"),
        legacy: args.iter().any(|a| a == "--legacy-search"),
        protocol: 3,
        replay_game_cap: argument(args, "--replay-game-cap", "256").parse()?,
        mask_cutoffs: match argument(args, "--cutoff-value", "mask").as_str() {
            "mask" => true,
            "draw" => false,
            _ => anyhow::bail!("--cutoff-value must be mask or draw"),
        },
    };
    anyhow::ensure!(settings.gpu_batch > 0, "GPU batch must be positive");
    let mut resident: HashMap<usize, (PathBuf, Network, Adam, Saved, Option<EvaluationJob>)> =
        HashMap::new();
    for _ in 0..if forever { usize::MAX } else { cycles } {
        for &players in &player_counts {
            if control.stopping() {
                return Ok(());
            }
            anyhow::ensure!(
                (2..=3).contains(&players),
                "training currently supports 2/3 players"
            );
            let curriculum = if players == 2 && args.iter().any(|a| a == "--curriculum-2p") {
                Some(crate::curriculum::load(
                    &root,
                    argument(args, "--rings-2p", "3").parse()?,
                    argument(args, "--curriculum-max-rings", "7").parse()?,
                )?)
            } else {
                None
            };
            if let Some(c) = &curriculum {
                atomic_json(&root.join("curriculum.json"), c)?;
            }
            let config = Config {
                players,
                rings: if let Some(c) = &curriculum {
                    c.rings
                } else {
                    argument(
                        args,
                        &format!("--rings-{players}p"),
                        &argument(args, "--rings", "4"),
                    )
                    .parse()?
                },
                blocks: argument(args, "--blocks", "4").parse()?,
                filters: argument(args, "--filters", "64").parse()?,
                sims: argument(args, "--sims", "64").parse()?,
                actors: argument(args, "--actors", "16").parse()?,
                max_steps: argument(args, "--max-steps", "4000").parse()?,
                repetition: argument(args, "--repetition", "3").parse()?,
                replay: argument(args, "--buffer", "5000").parse()?,
                batch: argument(args, "--batch-size", "64").parse()?,
                train_steps: argument(args, "--train-steps", "100").parse()?,
            };
            anyhow::ensure!(
                (3..=7).contains(&config.rings)
                    && (config.rings >= 4 || players == 2)
                    && config.sims > 0
                    && config.actors > 0
                    && config.max_steps > 0
                    && config.replay > 0
                    && config.batch > 0
                    && config.train_steps > 0
                    && config.filters > 0
                    && config.repetition != 1,
                "invalid configuration"
            );
            let model_name = if curriculum.is_some() || config.rings == 3 {
                format!("{players}p-r{}", config.rings)
            } else {
                format!("{players}p")
            };
            let dir = root.join(model_name);
            fs::create_dir_all(dir.join("games"))?;
            let board = Board::new(players, config.rings, false);
            let (net, mut adam, mut saved, mut evaluation) = if let Some((
                cached_dir,
                net,
                adam,
                saved,
                evaluation,
            )) = resident
                .remove(&players)
                .filter(|(path, _, _, _, _)| path == &dir)
            {
                let _ = cached_dir;
                (net, adam, saved, evaluation)
            } else {
                tch::manual_seed(players as i64 * 17);
                let mut net = Network::new(
                    players,
                    board.grid(),
                    board.action_size(),
                    config.blocks,
                    config.filters,
                    device,
                );
                let fresh = !dir.join("latest.json").exists();
                if fresh {
                    if let Some(path) = curriculum.as_ref().and_then(|c| c.source_model.as_ref()) {
                        let count = net.import_spatial_features(Path::new(path))?;
                        println!(
                            "Transferred {count} spatial tensors; initialized new dense heads for {} rings",
                            config.rings
                        );
                    }
                    if let Some(i) = args.iter().position(|a| a == "--warm-start") {
                        let weights =
                            PathBuf::from(args.get(i + 1).context("missing warm-start root")?)
                                .join(format!("{players}p.pt"));
                        anyhow::ensure!(
                            weights.exists(),
                            "missing warm-start weights: {}",
                            weights.display()
                        );
                        if weights.exists() {
                            net.vs.load(&weights)?;
                            println!("Imported weights: {}", weights.display());
                        }
                    }
                }
                let mut adam = Adam::new();
                let saved = load(
                    &dir,
                    config.clone(),
                    &mut net,
                    &mut adam,
                    players as u64 * 17,
                )?;
                let evaluation = EvaluationJob::load(&dir, &board, &config, device)?;
                (net, adam, saved, evaluation)
            };
            if saved.search_settings.as_ref() != Some(&settings) {
                for e in &mut saved.episodes {
                    e.tree = None;
                }
                for sample in &mut saved.replay {
                    sample.value_weight = if settings.mask_cutoffs
                        && (sample.termination.as_deref() == Some("max_steps")
                            || (sample.termination.is_none()
                                && sample.value.iter().all(|v| *v == 0.)))
                    {
                        0.
                    } else {
                        1.
                    };
                }
                saved.search_settings = Some(settings.clone());
            }
            atomic_json(&dir.join("config.json"), &config)?;
            if !dir.join("baseline.ot").exists() {
                net.vs.save(dir.join("baseline.tmp.ot"))?;
                fs::rename(dir.join("baseline.tmp.ot"), dir.join("baseline.ot"))?;
            }
            let concurrent_games: usize =
                argument(args, "--concurrent-games", &config.actors.to_string()).parse()?;
            anyhow::ensure!(concurrent_games > 0, "concurrent games must be positive");
            control.last = Instant::now();
            println!(
                "{}p iteration {}, {} concurrent games",
                players,
                saved.iteration + 1,
                concurrent_games
            );
            match iteration(
                &board,
                &dir,
                &mut saved,
                &net,
                &mut adam,
                &mut control,
                concurrent_games,
                &settings,
                &mut evaluation,
            ) {
                Ok(()) => {}
                Err(e) => {
                    save(&dir, &saved, &net, &adam)?;
                    if let Some(job) = evaluation.as_ref() {
                        job.persist()?;
                    }
                    status(
                        &dir,
                        &saved,
                        if control.stopping() {
                            "stopped"
                        } else {
                            "error"
                        },
                        0,
                        json!({"message":e.to_string()}),
                    )?;
                    if control.stopping() {
                        println!("Saved model, optimizer, replay and unfinished games; exiting.");
                        return Ok(());
                    }
                    return Err(e);
                }
            }
            let promotion_candidate = curriculum.as_ref().is_some_and(|c| c.rings < c.max_rings)
                && crate::curriculum::ready_to_test(&dir)?;
            let eval_every: u64 = argument(args, "--eval-every", "5").parse()?;
            if evaluation.is_none()
                && eval_every > 0
                && (saved.iteration == 1 || saved.iteration % eval_every == 0)
                && !control.stopping()
            {
                let per_seat: usize = argument(args, "--eval-games-per-seat", "2").parse()?;
                let per_seat = if promotion_candidate {
                    per_seat.max(16)
                } else {
                    per_seat
                };
                anyhow::ensure!(per_seat > 0, "evaluation games per seat must be positive");
                evaluation = Some(EvaluationJob::create(
                    &dir,
                    &board,
                    &config,
                    &net,
                    saved.iteration,
                    per_seat,
                    promotion_candidate,
                    &settings,
                )?);
            }
            if !forever {
                // Finite validation runs still produce a complete evaluation report.
                while evaluation.is_some() && !control.stopping() {
                    service_evaluation(&mut evaluation, &board, &dir, &settings, &mut control)?;
                }
            }
            if let Some(job) = evaluation.as_ref() {
                job.persist()?;
            }
            resident.insert(players, (dir, net, adam, saved, evaluation));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ogf_view_export_uses_ring_ids_and_saved_palette_without_mutating_checkpoint_frames() {
        assert_eq!(ring_label(0), "A");
        assert_eq!(ring_label(26), "AA");
        for players in [2, 3, 5] {
            let board = Board::new(players, 7, false);
            let episode = Episode::new(&board, 0, 0, &mut Random(17));
            let record = ogf(&board, &episode, 0, 0);
            assert_eq!(record["version"], 2);
            assert_eq!(record["profile"], "view");
            assert_eq!(record["board"]["center"], "A0");
            assert_eq!(record["board"]["ring-colors"].as_array().unwrap().len(), 7);
            assert!(record.get("colors").is_none());
            let old = json!({"elements":[["orb","eat","2:3",4]],"food":{"0:0":2}});
            let converted = view_frame(&old);
            assert_eq!(converted["elements"][0][2], "C3");
            assert_eq!(converted["food"]["A0"], 2);
            assert_eq!(old["elements"][0][2], "2:3");
        }
    }
    #[test]
    fn capped_game_sampling_covers_entire_game_and_resumes() {
        let a = game_sample_indices(4000, 256, &mut Random(17));
        assert_eq!(a.len(), 256);
        for (j, &i) in a.iter().enumerate() {
            assert!(i >= j * 4000 / 256 && i < (j + 1) * 4000 / 256);
        }
        assert_eq!(a, game_sample_indices(4000, 256, &mut Random(17)));
        assert_eq!(game_sample_indices(3, 256, &mut Random(17)), vec![0, 1, 2]);
        assert!(game_sample_indices(0, 256, &mut Random(17)).is_empty());
    }
    #[test]
    fn unknown_outcomes_have_no_value_gradient() {
        let v = Tensor::from_slice(&[0.5f32, -0.5, 0.8, -0.8])
            .view([2, 2])
            .set_requires_grad(true);
        let target = Tensor::zeros_like(&v);
        let loss = masked_value_loss(&v, &target, &Tensor::from_slice(&[1f32, 0.]));
        assert!((f64::try_from(&loss).unwrap() - 0.25).abs() < 1e-6);
        loss.backward();
        let gradient: Vec<f32> = Vec::try_from(v.grad().view([-1])).unwrap();
        assert_eq!(gradient, vec![0.5, -0.5, 0., 0.]);
        let empty = masked_value_loss(&v, &target, &Tensor::from_slice(&[0f32, 0.]));
        assert_eq!(f64::try_from(empty).unwrap(), 0.);
        let draw = masked_value_loss(&v, &target, &Tensor::from_slice(&[1f32, 1.]));
        assert!((f64::try_from(draw).unwrap() - 0.445).abs() < 1e-6);
    }
    #[test]
    fn live_feed_bounds_history_without_truncating_recordings() {
        let root = std::env::temp_dir().join(format!("organism-live-test-{}", std::process::id()));
        let dir = root.join("2p");
        fs::create_dir_all(&dir).unwrap();
        let board = Board::new(2, 4, false);
        let mut e = Episode::new(&board, 1, 1, &mut Random(0));
        e.frames = (0..100).map(|i| frame(&board, &e.state, i, None)).collect();
        publish_live(&dir, &board, &e, 1, 1, "evaluation").unwrap();
        let live: Value = serde_json::from_reader(std::io::BufReader::new(
            File::open(root.join("current.json")).unwrap(),
        ))
        .unwrap();
        assert_eq!(live["frames"].as_array().unwrap().len(), 64);
        assert_eq!(live["frames"][63]["step"], 99);
        assert_eq!(live["live-stage"], "evaluation");
        assert_eq!(
            ogf(&board, &e, 1, 1)["frames"].as_array().unwrap().len(),
            100
        );
        fs::remove_dir_all(root).unwrap();
    }
    #[test]
    fn metric_recovery_repairs_partial_tail_without_duplicate_iterations() {
        let dir = std::env::temp_dir().join(format!("organism-metric-test-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        fs::write(
            dir.join("metrics.jsonl"),
            "{\"iteration\":1}\n{\"iteration\":",
        )
        .unwrap();
        let metric = json!({"iteration":2,"updates":100});
        reconcile_metric(&dir, &metric).unwrap();
        reconcile_metric(&dir, &metric).unwrap();
        let rows: Vec<Value> = fs::read_to_string(dir.join("metrics.jsonl"))
            .unwrap()
            .lines()
            .filter_map(|s| serde_json::from_str(s).ok())
            .collect();
        assert_eq!(rows, vec![json!({"iteration":1}), metric]);
        fs::remove_dir_all(dir).unwrap();
    }
    #[test]
    fn exploration_covers_full_rounds_not_menu_choices() {
        let b = Board::new(2, 3, false);
        let mut state = b.initial();
        let settings = SearchSettings {
            exploration_rounds: 10,
            gpu_batch: 2,
            reuse: true,
            legacy: false,
            protocol: 3,
            replay_game_cap: 256,
            mask_cutoffs: true,
        };
        state.round = 9;
        assert!(settings.sample(&state, 1000));
        state.round = 10;
        assert!(!settings.sample(&state, 2));
    }
    #[test]
    fn background_candidate_is_frozen_and_job_is_durable() {
        let root = std::env::temp_dir().join(format!("organism-eval-job-{}", std::process::id()));
        fs::create_dir_all(&root).unwrap();
        let board = Board::new(2, 3, false);
        let config = Config {
            players: 2,
            rings: 3,
            blocks: 1,
            filters: 8,
            sims: 2,
            actors: 2,
            max_steps: 10,
            repetition: 3,
            replay: 20,
            batch: 4,
            train_steps: 1,
        };
        let net = EvaluationJob::network(&board, &config, Device::Cpu);
        net.vs.save(root.join("baseline.ot")).unwrap();
        let settings = SearchSettings {
            exploration_rounds: 10,
            gpu_batch: 2,
            reuse: true,
            legacy: false,
            protocol: 3,
            replay_game_cap: 256,
            mask_cutoffs: true,
        };
        let mut job =
            EvaluationJob::create(&root, &board, &config, &net, 7, 1, false, &settings).unwrap();
        let input = board.encode(&board.initial(), 0);
        let before = job.candidate.infer(&input, 1).unwrap();
        tch::no_grad(|| {
            for (_, mut v) in net.vs.variables() {
                let _ = v.fill_(0.123);
            }
        });
        assert_eq!(before, job.candidate.infer(&input, 1).unwrap());
        job.saved
            .session
            .tick(&board, &job.candidate, &job.opponent, 2, || Ok(()))
            .unwrap();
        job.persist().unwrap();
        let loaded = EvaluationJob::load(&root, &board, &config, Device::Cpu)
            .unwrap()
            .unwrap();
        assert_eq!(loaded.saved.iteration, 7);
        assert_eq!(loaded.saved.session.steps, job.saved.session.steps);
        assert_eq!(before, loaded.candidate.infer(&input, 1).unwrap());
        fs::remove_dir_all(root).unwrap();
    }
    #[test]
    fn food_accumulation_is_diagnostic_not_exact_repetition() {
        let b = Board::new(2, 3, false);
        let a = b.legal(&b.initial())[0].1.clone();
        let mut next = a.clone();
        next.pieces.iter_mut().flatten().next().unwrap().food += 1;
        assert!(same_layout(&a, &next));
        assert_ne!(repetition_state(&a), repetition_state(&next));
        next.pieces.iter_mut().flatten().next().unwrap().kind = crate::game::Kind::Circulate;
        assert!(!same_layout(&a, &next));
    }
    #[test]
    fn draw_only_iterations_cannot_anneal_learning_to_zero() {
        assert_eq!(learning_rate(0), 1e-3);
        assert_eq!(learning_rate(20), 5e-4);
        assert_eq!(learning_rate(80), 1e-4);
        assert_eq!(learning_rate(u64::MAX), 1e-4);
    }
}
