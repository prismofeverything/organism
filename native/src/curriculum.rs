//! Conservative board-size curriculum, with retained stages and transferable spatial features.
use crate::network::Network;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::{
    fs::{self, File},
    io::{BufRead, BufReader},
    path::Path,
};
#[derive(Serialize, Deserialize)]
pub struct Progress {
    pub version: u32,
    pub rings: usize,
    pub max_rings: usize,
    pub source_model: Option<String>,
    pub promotions: Vec<Value>,
}
pub fn load(root: &Path, initial: usize, max: usize) -> Result<Progress> {
    let file = root.join("curriculum.json");
    let p = if file.exists() {
        serde_json::from_reader(BufReader::new(File::open(file)?))?
    } else {
        Progress {
            version: 1,
            rings: initial,
            max_rings: max,
            source_model: None,
            promotions: vec![],
        }
    };
    anyhow::ensure!(
        p.version == 1 && (3..=7).contains(&p.rings) && p.rings <= p.max_rings && p.max_rings <= 7,
        "invalid curriculum"
    );
    anyhow::ensure!(
        p.max_rings == max,
        "curriculum maximum differs from saved configuration"
    );
    Ok(p)
}
/// Games in a promotion test, and the `--eval-games-per-seat` floor that
/// produces them. The screen below projects onto this same size.
pub const TEST_GAMES: usize = 32;
/// Evaluations older than this describe a model too far back to speak for the
/// current one. Iteration numbers also restart when a checkpoint is replaced,
/// so a window is what keeps a displaced lineage out of the evidence.
const EVIDENCE_WINDOW: u64 = 150;
/// A promotion test costs `TEST_GAMES` games at full search depth, and it runs
/// alongside self-play, so it is worth paying for only when recent evidence
/// makes passing plausible.
///
/// Self-play cannot supply that evidence. Both seats share one network, so the
/// fraction of self-play games that end in a win measures how decisively the
/// model beats itself, which is unrelated to whether it beats anything else:
/// this gate once fired for a model that went on to score 0 wins and 4 losses
/// in its first six test games. Measured play against the frozen baseline is
/// the only signal here that tracks the criterion being screened for.
pub fn ready_to_test(dir: &Path, iteration: u64) -> Result<bool> {
    let path = dir.join("metrics.jsonl");
    if !path.exists() {
        return Ok(false);
    }
    let rows: Vec<Value> = BufReader::new(File::open(path)?)
        .lines()
        .filter_map(|s| s.ok())
        .filter_map(|s| serde_json::from_str(&s).ok())
        .collect();
    Ok(active_rows(&rows) && worth_testing(&recent_evaluations(dir, iteration, 3)?))
}
/// Enough recent self-play to be worth measuring at all. Deliberately says
/// nothing about how those games ended.
fn active_rows(rows: &[Value]) -> bool {
    if rows.len() < 3 {
        return false;
    }
    rows[rows.len() - 3..]
        .iter()
        .map(|row| row["games"].as_array().map_or(0, Vec::len))
        .sum::<usize>()
        >= 48
}
/// Completed evaluation reports for this model, newest first.
fn recent_evaluations(dir: &Path, iteration: u64, want: usize) -> Result<Vec<Value>> {
    let path = dir.join("evaluations");
    if !path.exists() {
        return Ok(vec![]);
    }
    let mut reports: Vec<(u64, Value)> = vec![];
    for entry in fs::read_dir(&path)? {
        let entry = entry?;
        if entry.path().extension().is_none_or(|e| e != "json") {
            continue;
        }
        let Ok(report) =
            serde_json::from_reader::<_, Value>(BufReader::new(File::open(entry.path())?))
        else {
            continue;
        };
        let at = report["iteration"].as_u64().unwrap_or(0);
        if at <= iteration && iteration.saturating_sub(at) <= EVIDENCE_WINDOW {
            reports.push((at, report));
        }
    }
    reports.sort_by_key(|(at, _)| std::cmp::Reverse(*at));
    reports.truncate(want);
    Ok(reports.into_iter().map(|(_, report)| report).collect())
}
/// Would a promotion test that reproduced this evidence pass? Pooling the
/// recent reports and projecting them onto the test size keeps the screen
/// exactly as strict as the gate it is deciding whether to pay for, so
/// tightening `accepts` can never leave the screen waving candidates through.
fn worth_testing(reports: &[Value]) -> bool {
    let mut games = 0usize;
    let mut wins = 0usize;
    let mut cutoffs = 0usize;
    for report in reports {
        let n = report["games"].as_array().map_or(0, Vec::len);
        games += n;
        wins += report["wins"].as_u64().unwrap_or(0) as usize;
        cutoffs += report["cutoffs"].as_u64().unwrap_or(n as u64) as usize;
    }
    if games == 0 {
        return false;
    }
    let project = |n: usize| (n as f64 / games as f64 * TEST_GAMES as f64).round() as usize;
    accepts(project(wins), project(cutoffs), TEST_GAMES)
}
pub fn lower_bound(wins: usize, total: usize) -> f64 {
    if total == 0 {
        return 0.;
    }
    let n = total as f64;
    let p = wins as f64 / n;
    let z = 1.96f64;
    (p + z * z / (2. * n) - z * (p * (1. - p) / n + z * z / (4. * n * n)).sqrt()) / (1. + z * z / n)
}
/// The promotion criterion over raw counts: a full-size test, at most a quarter
/// of it unresolved, and a win rate whose 95% lower bound clears even odds.
fn accepts(wins: usize, cutoffs: usize, games: usize) -> bool {
    games >= TEST_GAMES && cutoffs * 4 <= games && lower_bound(wins, games) > 0.5
}
pub fn passes(report: &Value) -> bool {
    let n = report["games"].as_array().map_or(0, Vec::len);
    accepts(
        report["wins"].as_u64().unwrap_or(0) as usize,
        report["cutoffs"].as_u64().unwrap_or(n as u64) as usize,
        n,
    )
}
pub fn promote(root: &Path, progress: &mut Progress, net: &Network, report: &Value) -> Result<()> {
    anyhow::ensure!(
        progress.rings < progress.max_rings && passes(report),
        "curriculum criteria not met"
    );
    let dir = root.join("curriculum");
    fs::create_dir_all(&dir)?;
    let name = format!("2p-{}-to-{}.ot", progress.rings, progress.rings + 1);
    let path = dir.join(&name);
    let tmp = dir.join(format!("{name}.tmp"));
    net.vs.save(&tmp)?;
    File::open(&tmp)?.sync_all()?;
    fs::rename(tmp, &path)?;
    File::open(&dir)?.sync_all()?;
    progress.promotions.push(json!({"from_rings":progress.rings,"to_rings":progress.rings+1,"evaluation":report,"wilson_lower_bound":lower_bound(report["wins"].as_u64().context("wins")? as usize,report["games"].as_array().context("games")?.len())}));
    progress.rings += 1;
    progress.source_model = Some(path.to_string_lossy().into_owned());
    crate::train::atomic_json(&root.join("curriculum.json"), progress)?;
    File::open(root)?.sync_all()?;
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn qualified_stage_saves_transfer_source_and_persists_next_board() -> Result<()> {
        let root =
            std::env::temp_dir().join(format!("organism-curriculum-test-{}", std::process::id()));
        fs::create_dir_all(&root)?;
        let net = Network::new(2, 12, 53, 1, 8, tch::Device::Cpu);
        let mut p = load(&root, 3, 4)?;
        let report = json!({"games":vec![json!({});32],"wins":22,"cutoffs":0});
        promote(&root, &mut p, &net, &report)?;
        let restored = load(&root, 3, 4)?;
        assert_eq!(restored.rings, 4);
        assert_eq!(restored.promotions.len(), 1);
        assert!(Path::new(restored.source_model.as_ref().unwrap()).exists());
        let mut larger = Network::new(2, 18, 71, 1, 8, tch::Device::Cpu);
        assert!(
            larger.import_spatial_features(Path::new(restored.source_model.as_ref().unwrap()))? > 0
        );
        fs::remove_dir_all(root)?;
        Ok(())
    }
    #[test]
    fn activity_precondition_counts_games_not_how_they_ended() {
        let cutoffs = json!({"games":vec![json!({"termination":"repetition"});16]});
        let wins = json!({"games":vec![json!({"termination":"win"});16]});
        assert!(!active_rows(&[wins.clone(), wins.clone()]), "needs three rows");
        assert!(!active_rows(&[wins.clone(), wins.clone(), json!({"games":[]})]), "needs 48 games");
        // Self-play decisiveness must no longer decide anything on its own:
        // drawn and decisive self-play are equally uninformative about strength.
        assert!(active_rows(&[wins.clone(), cutoffs.clone(), wins.clone()]));
        assert!(active_rows(&[cutoffs.clone(), cutoffs.clone(), cutoffs]));
        assert!(active_rows(&[wins.clone(), wins.clone(), wins]));
    }
    #[test]
    fn screen_pays_for_a_test_only_when_measured_play_could_pass_it() {
        let report = |games: usize, wins: usize, cutoffs: usize| {
            json!({"games":vec![json!({});games],"wins":wins,"cutoffs":cutoffs})
        };
        assert!(!worth_testing(&[]), "no evidence must not open the gate");
        // The record that actually triggered the wasted test: 0 of 6, mostly cut off.
        assert!(!worth_testing(&[report(6, 0, 2)]));
        // Losing to the baseline is screened out however decisive the games are.
        assert!(!worth_testing(&[report(16, 4, 0)]));
        // So is a record that cannot clear the unresolved-game quarter.
        assert!(!worth_testing(&[report(16, 14, 6)]));
        // Just short of the projected lower bound, and just past it.
        assert!(!worth_testing(&[report(16, 10, 1)]));
        assert!(worth_testing(&[report(16, 11, 1)]));
        // Reports pool, so two small evaluations can carry the screen together.
        assert!(worth_testing(&[report(8, 6, 0), report(8, 6, 1)]));
    }
    #[test]
    fn readiness_reads_evaluations_and_ignores_a_displaced_lineage() -> Result<()> {
        let root = std::env::temp_dir().join(format!("organism-readiness-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("evaluations"))?;
        let row = json!({"games":vec![json!({"termination":"win"});16]});
        fs::write(root.join("metrics.jsonl"), format!("{row}\n{row}\n{row}\n"))?;
        assert!(
            !ready_to_test(&root, 550)?,
            "fully decisive self-play was the old gate's entire input; it must no longer qualify"
        );
        let record = |name: &str, at: u64, games: usize, wins: usize, cutoffs: usize| -> Result<()> {
            let report =
                json!({"iteration":at,"games":vec![json!({});games],"wins":wins,"cutoffs":cutoffs});
            fs::write(
                root.join("evaluations").join(name),
                serde_json::to_string(&report)?,
            )?;
            Ok(())
        };
        record("000540.json", 540, 6, 0, 2)?;
        assert!(!ready_to_test(&root, 550)?, "a losing evaluation must not qualify");
        record("000030.json", 30, 16, 16, 0)?;
        assert!(
            !ready_to_test(&root, 550)?,
            "a winning report from outside the evidence window belongs to another model"
        );
        record("000900.json", 900, 16, 16, 0)?;
        assert!(
            !ready_to_test(&root, 550)?,
            "iteration numbers restart on replacement; a future-dated report is a displaced lineage"
        );
        record("000545.json", 545, 16, 13, 1)?;
        record("000550.json", 550, 16, 13, 0)?;
        assert!(ready_to_test(&root, 550)?, "recent winning evaluations qualify");
        fs::remove_dir_all(root)?;
        Ok(())
    }
    #[test]
    fn gates_require_reliable_completion_and_seat_balanced_wins() {
        assert!(!passes(
            &json!({"games":vec![json!({});4],"wins":4,"cutoffs":0})
        ));
        assert!(!passes(
            &json!({"games":vec![json!({});32],"wins":21,"cutoffs":0})
        ));
        assert!(passes(
            &json!({"games":vec![json!({});32],"wins":22,"cutoffs":0})
        ));
        assert!(!passes(
            &json!({"games":vec![json!({});32],"wins":22,"cutoffs":10})
        ));
    }
}
