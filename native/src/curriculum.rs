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
pub fn ready_to_test(dir: &Path) -> Result<bool> {
    let path = dir.join("metrics.jsonl");
    if !path.exists() {
        return Ok(false);
    }
    let rows: Vec<Value> = BufReader::new(File::open(path)?)
        .lines()
        .filter_map(|s| s.ok())
        .filter_map(|s| serde_json::from_str(&s).ok())
        .collect();
    Ok(ready_rows(&rows))
}
fn ready_rows(rows: &[Value]) -> bool {
    if rows.len() < 3 {
        return false;
    }
    let recent = &rows[rows.len() - 3..];
    let mut count = 0;
    for row in recent {
        let Some(games) = row["games"].as_array() else {
            return false;
        };
        count += games.len();
        if games.is_empty()
            || games.iter().filter(|g| g["termination"] == "win").count() as f64
                / (games.len() as f64)
                < 0.6
        {
            return false;
        }
    }
    count >= 48
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
pub fn passes(report: &Value) -> bool {
    let n = report["games"].as_array().map_or(0, Vec::len);
    let wins = report["wins"].as_u64().unwrap_or(0) as usize;
    let cutoffs = report["cutoffs"].as_u64().unwrap_or(n as u64) as usize;
    n >= 32 && cutoffs * 4 <= n && lower_bound(wins, n) > 0.5
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
    fn gates_require_reliable_completion_and_seat_balanced_wins() {
        let cutoffs = json!({"games":vec![json!({"termination":"repetition"});16]});
        let wins = json!({"games":vec![json!({"termination":"win"});16]});
        assert!(!ready_rows(&[wins.clone(), wins.clone()]));
        assert!(!ready_rows(&[wins.clone(), cutoffs, wins.clone()]));
        assert!(ready_rows(&[wins.clone(), wins.clone(), wins]));
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
