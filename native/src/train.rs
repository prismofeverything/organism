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
    io::Write,
    path::{Path, PathBuf},
    sync::atomic::{AtomicBool, Ordering},
    time::{Instant, SystemTime, UNIX_EPOCH},
};
use tch::{Device, Kind, Tensor};
static STOPPING: AtomicBool = AtomicBool::new(false);
unsafe extern "C" {
    fn organism_cuda_fraction(f: f64) -> i32;
    fn organism_cuda_empty_cache();
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
fn atomic_json(path: &Path, data: &impl Serialize) -> Result<()> {
    let tmp = path.with_extension("json.tmp");
    let mut file = File::create(&tmp)?;
    serde_json::to_writer(&mut file, data)?;
    file.sync_all()?;
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
#[derive(Clone, Serialize, Deserialize)]
struct Sample {
    state: State,
    pi: Vec<f32>,
    value: Vec<f32>,
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
    #[serde(default)]
    decisions: usize,
}
pub(crate) fn repetition_state(s: &State) -> State {
    let mut s = s.clone();
    s.round = 0;
    s.next_order = 0;
    for p in s.pieces.iter_mut().flatten() {
        p.order = 0;
    }
    s
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
fn ogf_frames(
    board: &Board,
    e: &Episode,
    iteration: u64,
    number: usize,
    frames: &[Value],
) -> Value {
    let names = ["orb", "mass", "brone", "laam", "stuk"];
    let colors = ["orange", "blue", "purple", "red", "yellow"];
    let id = |i: usize| format!("{}:{}", board.spaces[i].0, board.spaces[i].1);
    json!({"format":"organism","version":1,"name":format!("organism_{}p-{}",board.players,e.id),"id":e.id,"iteration":iteration,"number":number,"source":"rust-self-play","frame-unit":"decision","started":e.started,"finished":if e.result.is_some(){Some(now())}else{None},"players":&names[..board.players],"colors":(0..board.players).map(|p|(names[p].to_string(),json!(colors[p]))).collect::<serde_json::Map<_,_>>(),"symmetry":board.symmetry,
        "board":{"center":id(0),"ring-colors":(0..board.rings).map(|r|r.to_string()).collect::<Vec<_>>(),"spaces":(0..board.spaces.len()).map(id).collect::<Vec<_>>(),"adjacencies":board.adj.iter().enumerate().map(|(i,adj)|(id(i),json!(adj.iter().map(|&i|id(i)).collect::<Vec<_>>()))).collect::<serde_json::Map<_,_>>()},
        "homes":board.homes.iter().enumerate().map(|(p,spaces)|(names[p].to_string(),json!(spaces.iter().map(|&i|id(i)).collect::<Vec<_>>()))).collect::<serde_json::Map<_,_>>(),"frames":frames,"result":e.result})
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
    let mut file = File::create(path.join("state.json"))?;
    serde_json::to_writer(&mut file, s)?;
    file.sync_all()?;
    File::open(path.join("model.ot"))?.sync_all()?;
    File::open(path.join("adam.ot"))?.sync_all()?;
    File::open(&path)?.sync_all()?;
    atomic_json(&dir.join("latest.json"), &json!({"generation":name}))?;
    File::open(dir)?.sync_all()?;
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
        });
    }
    let index: Value = serde_json::from_reader(File::open(dir.join("latest.json"))?)?;
    let name = index["generation"].as_str().context("invalid generation")?;
    anyhow::ensure!(
        !name.contains('/') && !name.contains(".."),
        "invalid snapshot path"
    );
    let generation = dir.join("snapshots").join(name);
    let raw: Value = serde_json::from_reader(File::open(generation.join("state.json"))?)?;
    let had_decisions = raw.get("decisions").is_some();
    let mut saved: Saved = serde_json::from_value(raw)?;
    if !had_decisions {
        saved.decisions = saved.episodes.iter().map(|e| e.samples.len()).sum();
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
    Ok(saved)
}
fn status(dir: &Path, s: &Saved, stage: &str, index: usize, extra: Value) -> Result<()> {
    let e = s.episodes.get(index);
    let mut value = json!({"stage":stage,"updated":now(),"pid":std::process::id(),"iteration":s.iteration+1,"game_number":index+1,"game_id":e.map(|e|&e.id),"started":e.map(|e|e.started),"step":e.map(|e|e.samples.len()),"actors":s.config.actors,"backend":"rust-libtorch"});
    if let Some(fields) = extra.as_object() {
        for (k, v) in fields {
            value[k] = v.clone();
        }
    }
    atomic_json(&dir.join("status.json"), &value)
}
fn finish_game(board: &Board, dir: &Path, s: &mut Saved, i: usize, reason: &str) -> Result<()> {
    let e = &mut s.episodes[i];
    let names = ["orb", "mass", "brone", "laam", "stuk"];
    let result = json!({"terminal":e.state.winner.is_some(),"steps":e.samples.len(),"winner":e.state.winner.map(|p|names[p]),"termination":reason});
    e.result = Some(result.clone());
    for sample in &mut e.samples {
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
fn iteration(
    board: &Board,
    dir: &Path,
    s: &mut Saved,
    net: &Network,
    adam: &mut Adam,
    control: &mut Control,
) -> Result<()> {
    while s.episodes.iter().filter(|e| e.result.is_some()).count() < s.config.actors
        && s.episodes.iter().filter(|e| e.result.is_none()).count() < s.config.actors
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
            while s.episodes.iter().filter(|e| e.result.is_none()).count() < s.config.actors {
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
                    json!({"active_games":active.len(),"phase":board.phase(&s.episodes[i].state)}),
                )?;
                telemetry = Instant::now();
            }
            let states: Vec<_> = active
                .iter()
                .map(|&i| s.episodes[i].state.clone())
                .collect();
            let saved_rng = s.rng.clone();
            let policies =
                match search::policies(board, &states, net, s.config.sims, &mut s.rng, true, || {
                    control.checkpoint()
                }) {
                    Ok(p) => p,
                    Err(e) => {
                        s.rng = saved_rng;
                        return Err(e);
                    }
                };
            s.decisions += active.len();
            for (&i, pi) in active.iter().zip(policies) {
                let e = &mut s.episodes[i];
                *seen[i].entry(repetition_state(&e.state)).or_default() += 1;
                let action = if e.samples.len() < 30 {
                    s.rng.sample(&pi)
                } else {
                    pi.iter()
                        .enumerate()
                        .max_by(|a, b| a.1.total_cmp(b.1))
                        .unwrap()
                        .0
                };
                let next = board
                    .legal(&e.state)
                    .into_iter()
                    .find(|(a, _)| *a == action)
                    .context("search chose illegal action")?
                    .1;
                e.samples.push(Sample {
                    state: e.state.clone(),
                    pi,
                    value: vec![],
                });
                e.state = next;
                e.frames
                    .push(frame(board, &e.state, e.samples.len(), Some(action)));
            }
            if checkpoint.elapsed().as_secs() >= 120 {
                for (e, map) in s.episodes.iter_mut().zip(&seen) {
                    e.seen = map.iter().map(|(s, n)| (s.clone(), *n)).collect();
                }
                s.elapsed += started.elapsed().as_secs_f64();
                started = Instant::now();
                save(dir, s, net, adam)?;
                checkpoint = Instant::now();
            }
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
                for _ in 0..count {
                    let sample = &s.replay[s.rng.index(s.replay.len())];
                    input.extend(board.encode(&sample.state, sample.state.player));
                    pi.extend_from_slice(&sample.pi);
                    value.extend_from_slice(&sample.value);
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
                let vl = v.mse_loss(&target_v, tch::Reduction::Mean);
                let p_value = f64::try_from(&pl)?;
                let v_value = f64::try_from(&vl)?;
                anyhow::ensure!(p_value.is_finite() && v_value.is_finite(), "nonfinite loss");
                adam.update(
                    net,
                    &(&pl + &vl),
                    1e-3 * 0.5f64.powi((s.iteration / 20) as i32),
                )?;
                s.policy_sum += p_value;
                s.value_sum += v_value;
                s.training_step += 1;
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
    let metrics = json!({"iteration":s.iteration+1,"game":format!("organism_{}p",board.players),"backend":"rust-libtorch","buffer":s.replay.len(),"policy_loss":s.policy_sum/s.training_step.max(1) as f64,"value_loss":s.value_sum/s.training_step.max(1) as f64,"total_seconds":s.elapsed,"games":stats,"optimizer_step":adam.step,"updates":s.training_step,"decisions":decisions,"games_per_hour":stats.len() as f64*3600./seconds,"rule_victories_per_hour":victories as f64*3600./seconds,"decisions_per_second":decisions as f64/seconds,"updates_per_hour":s.training_step as f64*3600./seconds});
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
    for _ in 0..if forever { usize::MAX } else { cycles } {
        for &players in &player_counts {
            if control.stopping() {
                return Ok(());
            }
            anyhow::ensure!(
                (2..=3).contains(&players),
                "training currently supports 2/3 players"
            );
            let config = Config {
                players,
                rings: argument(args, "--rings", "4").parse()?,
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
                (4..=7).contains(&config.rings)
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
            let dir = root.join(format!("{players}p"));
            fs::create_dir_all(dir.join("games"))?;
            let board = Board::new(players, config.rings, false);
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
            let mut saved = load(
                &dir,
                config.clone(),
                &mut net,
                &mut adam,
                players as u64 * 17,
            )?;
            atomic_json(&dir.join("config.json"), &config)?;
            if !dir.join("baseline.ot").exists() {
                net.vs.save(dir.join("baseline.tmp.ot"))?;
                fs::rename(dir.join("baseline.tmp.ot"), dir.join("baseline.ot"))?;
            }
            control.last = Instant::now();
            println!(
                "{}p iteration {}, {} concurrent games",
                players,
                saved.iteration + 1,
                config.actors
            );
            match iteration(&board, &dir, &mut saved, &net, &mut adam, &mut control) {
                Ok(()) => {}
                Err(e) => {
                    save(&dir, &saved, &net, &adam)?;
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
            let eval_every: u64 = argument(args, "--eval-every", "5").parse()?;
            if eval_every > 0
                && (saved.iteration == 1 || saved.iteration % eval_every == 0)
                && !control.stopping()
            {
                status(
                    &dir,
                    &saved,
                    "evaluation",
                    0,
                    json!({"iteration":saved.iteration}),
                )?;
                let mut opponent = Network::new(
                    players,
                    board.grid(),
                    board.action_size(),
                    config.blocks,
                    config.filters,
                    device,
                );
                opponent.vs.load(dir.join("baseline.ot"))?;
                let per_seat = argument(args, "--eval-games-per-seat", "2").parse()?;
                anyhow::ensure!(per_seat > 0, "evaluation games per seat must be positive");
                let mut watched = usize::MAX;
                let mut live = Episode::new(&board, saved.iteration, 1, &mut Random(0));
                let mut published = Instant::now() - std::time::Duration::from_secs(1);
                match crate::evaluate::run(
                    &board,
                    &net,
                    &opponent,
                    config.sims,
                    per_seat,
                    config.max_steps,
                    config.repetition,
                    9917,
                    || control.checkpoint(),
                    |states, steps, results| {
                        let i = if watched < states.len() && results[watched].is_none() {
                            watched
                        } else {
                            results.iter().position(Option::is_none).unwrap_or(0)
                        };
                        if watched != i {
                            watched = i;
                            live.frames.clear();
                            live.id = format!("eval-{:06}-{}", saved.iteration, i + 1);
                            published = Instant::now() - std::time::Duration::from_secs(1);
                        }
                        if live.frames.last().and_then(|f| f["step"].as_u64())
                            != Some(steps[i] as u64)
                        {
                            live.frames.push(frame(&board, &states[i], steps[i], None));
                            if live.frames.len() > 64 {
                                live.frames.remove(0);
                            }
                        }
                        live.state = states[i].clone();
                        live.result = results[i].clone();
                        if published.elapsed().as_secs_f64() >= 0.25
                            || results.iter().all(Option::is_some)
                        {
                            publish_live(
                                &dir,
                                &board,
                                &live,
                                saved.iteration,
                                i + 1,
                                "evaluation",
                            )?;
                            status(
                                &dir,
                                &saved,
                                "evaluation",
                                0,
                                json!({"iteration":saved.iteration,"game_id":live.id,"game_number":i+1,"step":steps[i],"active_games":results.iter().filter(|r|r.is_none()).count()}),
                            )?;
                            published = Instant::now();
                        }
                        Ok(())
                    },
                ) {
                    Ok(mut report) => {
                        report["iteration"] = json!(saved.iteration);
                        report["opponent"] = json!("frozen starting weights");
                        atomic_json(&dir.join("evaluation.json"), &report)?;
                        fs::create_dir_all(dir.join("evaluations"))?;
                        atomic_json(
                            &dir.join("evaluations")
                                .join(format!("{:06}.json", saved.iteration)),
                            &report,
                        )?;
                        println!("{}p evaluation: {}", players, report);
                        status(
                            &dir,
                            &saved,
                            "evaluation_complete",
                            0,
                            json!({"iteration":saved.iteration}),
                        )?;
                    }
                    Err(e) if control.stopping() => {
                        status(
                            &dir,
                            &saved,
                            "stopped",
                            0,
                            json!({"message":"Stopped during evaluation; training checkpoint already saved"}),
                        )?;
                        println!("Evaluation interrupted: {e}");
                        return Ok(());
                    }
                    Err(e) => return Err(e),
                }
            }
            drop(saved);
            drop(adam);
            drop(net);
            if !cpu {
                unsafe {
                    organism_cuda_empty_cache();
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn live_feed_bounds_history_without_truncating_recordings() {
        let root = std::env::temp_dir().join(format!("organism-live-test-{}", std::process::id()));
        let dir = root.join("2p");
        fs::create_dir_all(&dir).unwrap();
        let board = Board::new(2, 4, false);
        let mut e = Episode::new(&board, 1, 1, &mut Random(0));
        e.frames = (0..100).map(|i| frame(&board, &e.state, i, None)).collect();
        publish_live(&dir, &board, &e, 1, 1, "evaluation").unwrap();
        let live: Value =
            serde_json::from_reader(File::open(root.join("current.json")).unwrap()).unwrap();
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
}
