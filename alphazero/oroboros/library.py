"""Game library: persistent collection of discovered games.

Games are stored in a JSON file, sorted by richness score.
New games from search runs are merged in, deduplicating by rule structure.
The website reads this file to display the game catalog.
"""

from __future__ import annotations
import json
import os
from pathlib import Path

LIBRARY_PATH = Path("game_library.json")


def load_library(path: Path = LIBRARY_PATH) -> list[dict]:
    """Load the game library from disk."""
    if path.exists():
        with open(path) as f:
            return json.load(f)
    return []


def save_library(games: list[dict], path: Path = LIBRARY_PATH):
    """Save the game library to disk, sorted by richness."""
    games.sort(key=lambda g: g.get("richness", 0), reverse=True)
    with open(path, "w") as f:
        json.dump(games, f, indent=2, default=str)


def _game_fingerprint(game: dict) -> str:
    """Hash-like fingerprint for deduplication based on rule structure."""
    rs = game.get("ruleset", {})
    schema = rs.get("schema", {})
    parts = [
        schema.get("topology_type", ""),
        str(schema.get("topology_size", "")),
        str(schema.get("num_piece_types", "")),
        str(schema.get("num_players", "")),
        str(len(rs.get("rules", []))),
        str(rs.get("conflict_table", [])),
    ]
    # Include rule predicates for uniqueness
    for rule in rs.get("rules", [])[:5]:
        parts.append(str(rule.get("predicate", [])))
    return "|".join(parts)


def merge_into_library(new_games: list[dict], path: Path = LIBRARY_PATH) -> int:
    """Merge new games into the library, deduplicating. Returns count added."""
    library = load_library(path)
    existing_fps = {_game_fingerprint(g) for g in library}
    added = 0
    for game in new_games:
        fp = _game_fingerprint(game)
        if fp not in existing_fps:
            library.append(game)
            existing_fps.add(fp)
            added += 1
    save_library(library, path)
    return added


def import_from_search(search_dir: str, path: Path = LIBRARY_PATH) -> int:
    """Import top games from a search results directory."""
    top_path = os.path.join(search_dir, "top_games.json")
    if not os.path.exists(top_path):
        # Try checkpoints
        ckpt_dir = os.path.join(search_dir, "checkpoints")
        if os.path.exists(ckpt_dir):
            files = sorted(os.listdir(ckpt_dir))
            if files:
                last = os.path.join(ckpt_dir, files[-1])
                with open(last) as f:
                    data = json.load(f)
                games = []
                for entry in data.get("archive", data.get("population", [])):
                    rs = entry.get("ruleset", {})
                    games.append({
                        "richness": entry.get("richness", 0),
                        "summary": entry.get("summary", ""),
                        "ruleset": rs,
                    })
                return merge_into_library(games, path)
        return 0

    with open(top_path) as f:
        games = json.load(f)
    return merge_into_library(games, path)
