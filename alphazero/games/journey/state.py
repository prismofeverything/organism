"""Journey game state — Python port of journey/game.cljc.

States are plain dicts (immutable by convention; always copy-on-write).
Use copy.deepcopy() when mutating nested structures, or the helper `update_in`.

Key state shape (mirrors Clojure):
{
  "board":          { (q,r): tile, ... }
  "bag":            { color: count, ... }
  "deck":           [ card, ... ]
  "discard":        [ card, ... ]
  "ark":            (q,r)
  "neutral_tower":  (q,r)
  "heading_token":  (q,r)
  "captain_flame":  player_str
  "cipher":         { (q,r): { "colors": { color: { player: count, ... }, ... } }, ... }
  "pending_cipher": [ ... ]
  "flares_drawn":   int
  "players":        { player: player_state, ... }
  "turn_order":     [ player, ... ]
  "round":          int
  "wrapping_radius": int
  "player_turn":    { "player": str, "phase": str, ... }
  "game_over":      None | { "type": str, ... }
}

tile:
{
  "color":    color_str,
  "world":    True,
  "station":  None | { "type": str, "player": str, "level": int }
  "beacon":   None | player_str,
  "sundivers": { player: count, ... }
}

player_state:
{
  "habitat":   { "sundivers": int }
  "movement":  int (3-8)
  "stations":  { (q,r): { "type": str, "level": int }, ... }
  "gates":     { (q,r): { (q,r), ... }, ... }
  "reserve":   { "sundivers": int, "foundries": int, "matrixes": int,
                 "towers": int, "gates": int, "beacons": int, "level_platforms": int }
  "held_card": None | card
}
"""

from __future__ import annotations

import copy
import random
from typing import Any

from . import hex as hx

# ── constants ──────────────────────────────────────────────────────────────────

TILE_COLORS = ["sun", "silver", "green", "blue", "purple", "void"]
NUM_WORLDS_PER_COLOR = 13

CARD_SUITS = 5
CARDS_PER_SUIT = 13

MOVEMENT_MIN = 3
MOVEMENT_MAX = 8

# ── helpers ────────────────────────────────────────────────────────────────────

def deep_copy(state: dict) -> dict:
    return copy.deepcopy(state)


def get_in(obj: Any, path: list) -> Any:
    """Navigate nested dict/list by path; return None if any key missing."""
    cur = obj
    for key in path:
        if cur is None:
            return None
        if isinstance(cur, dict):
            cur = cur.get(key)
        elif isinstance(cur, list):
            try:
                cur = cur[key]
            except (IndexError, TypeError):
                return None
        else:
            return None
    return cur


def assoc_in(obj: Any, path: list, value: Any) -> Any:
    """Return a new nested structure with value set at path (copy-on-write)."""
    obj = copy.deepcopy(obj)
    cur = obj
    for key in path[:-1]:
        if isinstance(cur, dict):
            if key not in cur or not isinstance(cur[key], (dict, list)):
                cur[key] = {}
            cur = cur[key]
        elif isinstance(cur, list):
            cur = cur[key]
    last = path[-1]
    if isinstance(cur, dict):
        cur[last] = value
    elif isinstance(cur, list):
        cur[last] = value
    return obj


def update_in(obj: Any, path: list, fn, *args) -> Any:
    """Return new structure with fn applied to the value at path."""
    old = get_in(obj, path)
    return assoc_in(obj, path, fn(old, *args))


# ── world bag ─────────────────────────────────────────────────────────────────

def full_bag() -> dict[str, int]:
    return {color: NUM_WORLDS_PER_COLOR for color in TILE_COLORS}


def draw_from_bag(bag: dict[str, int]) -> tuple[dict[str, int], str]:
    """Draw a random color from the bag.  Returns (new_bag, color)."""
    choices = [color for color, n in bag.items() for _ in range(n)]
    if not choices:
        raise ValueError("Bag is empty")
    color = random.choice(choices)
    new_bag = dict(bag)
    new_bag[color] -= 1
    return new_bag, color


# ── deck ──────────────────────────────────────────────────────────────────────

def make_card(suit: int, value: int) -> dict:
    return {"suit": suit, "value": value}


def base_deck() -> list[dict]:
    return [make_card(s, v) for s in range(CARD_SUITS) for v in range(1, CARDS_PER_SUIT + 1)]


# ── cipher ────────────────────────────────────────────────────────────────────

def initial_cipher() -> dict:
    cipher = {(0, 0): {"colors": {}}}
    for direction, color in zip(hx.HEX_DIRECTIONS, TILE_COLORS):
        cipher[direction] = {"colors": {color: {}}}
    return cipher


# ── tile / board ──────────────────────────────────────────────────────────────

def make_tile(color: str) -> dict:
    return {
        "color": color,
        "world": True,
        "station": None,
        "beacon": None,
        "sundivers": {},
    }


def add_station(tile: dict, station_type: str, player: str, level: int) -> dict:
    tile = dict(tile)
    tile["station"] = {"type": station_type, "player": player, "level": level}
    return tile


# ── player ────────────────────────────────────────────────────────────────────

def initial_player() -> dict:
    return {
        "habitat": {"sundivers": 8},
        "movement": MOVEMENT_MIN,
        "stations": {},
        "gates": {},
        "reserve": {
            "sundivers": 5,
            "foundries": 3,
            "matrixes": 3,
            "towers": 3,
            "gates": 8,
            "beacons": 21,
            "level_platforms": 13,
        },
        "held_card": None,
    }


# ── initial state ─────────────────────────────────────────────────────────────

def initial_state(turn_order: list[str]) -> dict:
    bag = full_bag()
    bag, first_world = draw_from_bag(bag)
    first_tile = make_tile(first_world)
    first_tile = add_station(first_tile, "tower", "NEUTRAL", 0)

    deck = base_deck()
    random.shuffle(deck)

    return {
        "board": {(0, 0): first_tile},
        "bag": bag,
        "deck": deck,
        "discard": [],
        "ark": (0, 0),
        "neutral_tower": (0, 0),
        "heading_token": (0, 1),
        "captain_flame": turn_order[-1],
        "cipher": initial_cipher(),
        "pending_cipher": [],
        "flares_drawn": 0,
        "players": {p: initial_player() for p in turn_order},
        "turn_order": list(turn_order),
        "round": 0,
        "wrapping_radius": hx.DEFAULT_WRAPPING_RADIUS,
        "player_turn": {
            "player": turn_order[0],
            "phase": "choose_action_type",
        },
        "game_over": None,
    }


# ── accessors ─────────────────────────────────────────────────────────────────

def get_tile(state: dict, pos: tuple) -> dict | None:
    return state["board"].get(pos)


def current_player(state: dict) -> str | None:
    if state.get("game_over"):
        return None
    return get_in(state, ["player_turn", "player"])


def current_phase(state: dict) -> str | None:
    return get_in(state, ["player_turn", "phase"])


def heading_direction(state: dict) -> tuple[int, int]:
    return hx.heading_direction(state["ark"], state["heading_token"])


def launch_positions(state: dict) -> list[tuple]:
    ark = state["ark"]
    d = heading_direction(state)
    return [
        ark,
        hx.add(ark, d),
        hx.add(ark, hx.rotate_ccw(d)),
        hx.add(ark, hx.rotate_cw(d)),
    ]


# ── immobility ────────────────────────────────────────────────────────────────

def immobile(state: dict, player: str, pos: tuple) -> bool:
    return (player, pos) in (get_in(state, ["player_turn", "immobile"]) or set())


def mark_immobile(state: dict, player: str, pos: tuple) -> dict:
    state = deep_copy(state)
    immobile_set = get_in(state, ["player_turn", "immobile"]) or set()
    immobile_set = set(immobile_set) | {(player, pos)}
    state["player_turn"]["immobile"] = immobile_set
    return state


# ── explore / launch / fly ────────────────────────────────────────────────────

def explore(state: dict, player: str, pos: tuple) -> dict:
    """Draw a world tile from the bag, place it at pos, put 1 sundiver there."""
    bag, color = draw_from_bag(state["bag"])
    tile = make_tile(color)
    tile["sundivers"][player] = 1
    state = deep_copy(state)
    state["bag"] = bag
    state["board"][pos] = tile
    return mark_immobile(state, player, pos)


def launch_sundiver(state: dict, player: str, pos: tuple) -> dict:
    """Move one sundiver from habitat to pos; explore if tile absent."""
    state = deep_copy(state)
    state["players"][player]["habitat"]["sundivers"] -= 1
    if get_tile(state, pos):
        board = state["board"]
        tile = dict(board[pos])
        tile["sundivers"] = dict(tile["sundivers"])
        tile["sundivers"][player] = tile["sundivers"].get(player, 0) + 1
        board[pos] = tile
    else:
        state = explore(state, player, pos)
    return state


def gate_owner(state: dict, from_pos: tuple, to_pos: tuple) -> str | None:
    for player in state["turn_order"]:
        gates = get_in(state, ["players", player, "gates"]) or {}
        if to_pos in gates.get(from_pos, set()):
            return player
    return None


def fly_through_gate(state: dict, player: str, from_pos: tuple, to_pos: tuple) -> dict:
    """Move sundiver through an existing gate; gate owner gains reserve→habitat sundiver."""
    owner = gate_owner(state, from_pos, to_pos)
    state = deep_copy(state)
    board = state["board"]

    ftile = dict(board[from_pos])
    ftile["sundivers"] = dict(ftile["sundivers"])
    ftile["sundivers"][player] -= 1
    board[from_pos] = ftile

    ttile = dict(board[to_pos])
    ttile["sundivers"] = dict(ttile["sundivers"])
    ttile["sundivers"][player] = ttile["sundivers"].get(player, 0) + 1
    board[to_pos] = ttile

    if owner and owner != player:
        state["players"][owner]["reserve"]["sundivers"] -= 1
        state["players"][owner]["habitat"]["sundivers"] += 1

    return state


def fly_sundiver(state: dict, player: str, from_pos: tuple, to_pos: tuple) -> dict:
    from_tile = get_tile(state, from_pos)
    to_tile = get_tile(state, to_pos)
    if to_tile is None:
        state = deep_copy(state)
        board = state["board"]
        ftile = dict(board[from_pos])
        ftile["sundivers"] = dict(ftile["sundivers"])
        ftile["sundivers"][player] = ftile["sundivers"].get(player, 0) - 1
        board[from_pos] = ftile
        return explore(state, player, to_pos)
    elif from_tile["color"] == to_tile["color"]:
        state = deep_copy(state)
        board = state["board"]
        ftile = dict(board[from_pos])
        ftile["sundivers"] = dict(ftile["sundivers"])
        ftile["sundivers"][player] -= 1
        board[from_pos] = ftile
        ttile = dict(board[to_pos])
        ttile["sundivers"] = dict(ttile["sundivers"])
        ttile["sundivers"][player] = ttile["sundivers"].get(player, 0) + 1
        board[to_pos] = ttile
        return state
    else:
        return add_gate(state, player, from_pos, to_pos)


def add_gate(state: dict, player: str, from_pos: tuple, to_pos: tuple) -> dict:
    state = deep_copy(state)
    board = state["board"]
    ftile = dict(board[from_pos])
    ftile["sundivers"] = dict(ftile["sundivers"])
    ftile["sundivers"][player] = ftile["sundivers"].get(player, 0) - 1
    board[from_pos] = ftile
    pstate = state["players"][player]
    pstate["reserve"]["sundivers"] = pstate["reserve"].get("sundivers", 0) + 1
    pstate["reserve"]["gates"] -= 1
    gates = pstate.setdefault("gates", {})
    gates.setdefault(from_pos, set()).add(to_pos)
    gates.setdefault(to_pos, set()).add(from_pos)
    pt = state["player_turn"].setdefault("action", {})
    pt["gates_created"] = pt.get("gates_created", 0) + 1
    return state


# ── conversions ───────────────────────────────────────────────────────────────

def find_conversions(state: dict, player: str) -> list[dict]:
    """Return list of valid conversion dicts for player.

    Each conversion: {"type": str, "target": pos, "sundivers": [pos, ...]}

    Foundry (3 sundivers in a triangle → foundry station):
      Three mutually adjacent positions all having the same color and ≥1 sundiver.
    Matrix (2 sundivers in a line → matrix station):
      Two adjacent same-color positions each with ≥1 sundiver.
    Gate (2 sundivers on different colors → gate):
      Already handled by fly_sundiver.

    This is a simplified stub — the full rule set is in the Clojure source.
    """
    conversions = []
    board = state["board"]
    positions_with_sundiver = [
        pos for pos, tile in board.items()
        if tile["sundivers"].get(player, 0) >= 1
    ]

    # Foundry: triangle of 3 same-color positions
    pos_set = set(positions_with_sundiver)
    for pos in positions_with_sundiver:
        color = board[pos]["color"]
        nbrs = [n for n in hx.neighbors(pos) if n in pos_set and board.get(n, {}).get("color") == color]
        for n1 in nbrs:
            for n2 in nbrs:
                if n1 < n2 and n2 in set(hx.neighbors(n1)):
                    conversions.append({
                        "type": "foundry",
                        "target": pos,
                        "sundivers": sorted([pos, n1, n2]),
                    })

    # Matrix: pair of same-color adjacent positions
    for pos in positions_with_sundiver:
        color = board[pos]["color"]
        for nbr in hx.neighbors(pos):
            if nbr in pos_set and board.get(nbr, {}).get("color") == color and nbr > pos:
                conversions.append({
                    "type": "matrix",
                    "target": pos,
                    "sundivers": [pos, nbr],
                })

    return conversions


def convert(state: dict, player: str, station_type: str, target: tuple, sundivers: list) -> dict:
    """Consume sundivers and place a station.  Simplified stub."""
    state = deep_copy(state)
    board = state["board"]
    for pos in sundivers:
        tile = dict(board[pos])
        tile["sundivers"] = dict(tile["sundivers"])
        tile["sundivers"][player] -= 1
        board[pos] = tile
    # Place station
    target_tile = dict(board[target])
    target_tile["station"] = {"type": station_type, "player": player, "level": 1}
    board[target] = target_tile
    state["players"][player]["stations"][target] = {"type": station_type, "level": 1}
    state["player_turn"]["phase"] = "draw_cards"
    return state


# ── scoring ───────────────────────────────────────────────────────────────────

def compute_scores(state: dict, pos: tuple) -> dict[str, int]:
    """Score beacons for landing at pos (mirrors Clojure compute-scores)."""
    board = state["board"]
    cipher = state["cipher"]
    landed_color = board.get(pos, {}).get("color")
    scores: dict[str, int] = {}

    def add_beacons(s: dict, beacons: dict):
        for p, n in beacons.items():
            s[p] = s.get(p, 0) + n

    add_beacons(scores, get_in(cipher, [(0, 0), "colors", landed_color]) or {})
    for direction in hx.HEX_DIRECTIONS:
        neighbor = hx.add(pos, direction)
        neighbor_color = board.get(neighbor, {}).get("color")
        if neighbor_color:
            cipher_entry = get_in(cipher, [direction, "colors", neighbor_color]) or {}
            add_beacons(scores, cipher_entry)
    return scores


def land_ark(state: dict, pos: tuple) -> dict:
    scores = compute_scores(state, pos)
    max_score = max(scores.values()) if scores else 0
    winners = [p for p in state["turn_order"] if scores.get(p, 0) == max_score]
    state = deep_copy(state)
    state["game_over"] = {"type": "landing", "tile": pos, "scores": scores, "winners": winners}
    state["player_turn"]["phase"] = "game_over"
    return state


def available_landings(state: dict) -> list[tuple]:
    return [pos for pos in state["board"] if can_land_at(state, pos)]


def count_cipher_matches(state: dict, pos: tuple) -> int:
    board = state["board"]
    cipher = state["cipher"]
    count = 0
    for direction in hx.HEX_DIRECTIONS:
        neighbor = hx.add(pos, direction)
        neighbor_color = board.get(neighbor, {}).get("color")
        if neighbor_color and neighbor_color in get_in(cipher, [direction, "colors"] or {}):
            count += 1
    return count


def can_land_at(state: dict, pos: tuple) -> bool:
    center_colors = get_in(state, ["cipher", (0, 0), "colors"]) or {}
    tile_color = state["board"].get(pos, {}).get("color")
    ark = state["ark"]
    if not (tile_color and tile_color in center_colors):
        return False
    matches = count_cipher_matches(state, pos)
    if matches == 5 and ark == pos:
        return True
    if matches == 6 and (ark == pos or ark in hx.neighbors(pos)):
        return True
    return False


# ── end-of-turn helpers ───────────────────────────────────────────────────────

def advance_turn(state: dict) -> dict:
    """Move to next player; increment round when we wrap."""
    state = deep_copy(state)
    order = state["turn_order"]
    cur = state["player_turn"]["player"]
    idx = order.index(cur)
    next_idx = (idx + 1) % len(order)
    state["player_turn"] = {
        "player": order[next_idx],
        "phase": "choose_action_type",
    }
    if next_idx == 0:
        state["round"] = state.get("round", 0) + 1
    return state


def move_points(state: dict, player: str) -> int:
    """movement stat + 1 per distinct tile color the player has a station on."""
    pstate = state["players"][player]
    base = pstate.get("movement", MOVEMENT_MIN)
    board = state["board"]
    colors = {board[pos]["color"] for pos in pstate["stations"] if pos in board}
    return base + len(colors)
