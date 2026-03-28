"""Lineage tracker: records every game ever evaluated and its family tree.

Each record stores:
  - id: unique integer
  - parent_id: who spawned this (-1 for random seeds)
  - cycle: when it was created
  - richness: evaluation score
  - region: topology+size+types+rules key
  - topology: base topology type
  - alive: whether it's currently in the population
  - ruleset_hash: short hash for deduplication display

Stored in lineage.json, grows monotonically (never pruned).
"""

from __future__ import annotations
import hashlib
import json
from pathlib import Path

LINEAGE_PATH = Path("lineage.json")


def _ruleset_hash(ruleset_dict: dict) -> str:
    """Short hash of the ruleset for display."""
    raw = json.dumps(ruleset_dict, sort_keys=True, default=str)
    return hashlib.md5(raw.encode()).hexdigest()[:8]


def _topology_type(ruleset_dict: dict) -> str:
    schema = ruleset_dict.get("schema", {})
    return schema.get("topology_type", "?")


def load_lineage(path: Path = LINEAGE_PATH) -> dict:
    """Load lineage data. Returns {records: [...], next_id: int}."""
    if path.exists():
        with open(path) as f:
            return json.load(f)
    return {"records": [], "next_id": 0}


def save_lineage(data: dict, path: Path = LINEAGE_PATH):
    with open(path, "w") as f:
        json.dump(data, f, separators=(",", ":"))  # compact


def record_birth(lineage: dict, parent_id: int, cycle: int,
                 ruleset_dict: dict, richness: float = 0.0,
                 region: str = "") -> int:
    """Record a new organism birth. Returns its id."""
    oid = lineage["next_id"]
    lineage["next_id"] = oid + 1
    lineage["records"].append({
        "id": oid,
        "parent": parent_id,
        "cycle": cycle,
        "richness": richness,
        "region": region,
        "topology": _topology_type(ruleset_dict),
        "hash": _ruleset_hash(ruleset_dict),
        "alive": True,
    })
    return oid


def update_richness(lineage: dict, oid: int, richness: float):
    """Update richness after evaluation."""
    for r in lineage["records"]:
        if r["id"] == oid:
            r["richness"] = richness
            return


def mark_dead(lineage: dict, oid: int):
    """Mark an organism as no longer in population."""
    for r in lineage["records"]:
        if r["id"] == oid:
            r["alive"] = False
            return


def mark_all_dead(lineage: dict):
    """Mark all organisms as dead (before refreshing alive status)."""
    for r in lineage["records"]:
        r["alive"] = False


def mark_alive(lineage: dict, oid: int):
    """Mark an organism as currently alive in population."""
    for r in lineage["records"]:
        if r["id"] == oid:
            r["alive"] = True
            return


def export_for_visualization(lineage: dict) -> list[dict]:
    """Export lineage records for the web visualization."""
    return lineage["records"]
