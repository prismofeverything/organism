"""Journey game state — Python port of journey/game.cljc.

States are plain dicts (immutable by convention; always copy-on-write).
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
FLARE_SUIT = 4

STATION_RESERVE_KEY = {"foundry": "foundries", "matrix": "matrixes", "tower": "towers"}

ACTIVATION_ACTIONS_TABLE = {
    0: {"base": 1, "bonus": 0},
    1: {"base": 1, "bonus": 1},
    2: {"base": 2, "bonus": 1},
    3: {"base": 3, "bonus": 2},
}

ACTION_TYPES = ["move", "convert", "activate"]

# ── dict helpers ───────────────────────────────────────────────────────────────

def deep_copy(state: dict) -> dict:
    return copy.deepcopy(state)


def get_in(obj: Any, path: list) -> Any:
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
    old = get_in(obj, path)
    return assoc_in(obj, path, fn(old, *args))


# ── world bag ─────────────────────────────────────────────────────────────────

def full_bag() -> dict[str, int]:
    return {color: NUM_WORLDS_PER_COLOR for color in TILE_COLORS}


def draw_from_bag(bag: dict[str, int]) -> tuple[dict[str, int], str]:
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


def flare(card: dict) -> bool:
    return card["suit"] == FLARE_SUIT


def draw_n_cards(state: dict, n: int) -> tuple[dict, list]:
    """Draw n cards, reshuffling discard into deck if needed. Returns (state, cards)."""
    state = deep_copy(state)
    drawn = []
    for _ in range(n):
        if not state["deck"]:
            state["deck"] = state["discard"][:]
            random.shuffle(state["deck"])
            state["discard"] = []
        if state["deck"]:
            card = state["deck"].pop(0)
            state["discard"].append(card)
            drawn.append(card)
    return state, drawn


# ── cipher ────────────────────────────────────────────────────────────────────

def initial_cipher() -> dict:
    cipher = {(0, 0): {"colors": {}}}
    for direction, color in zip(hx.HEX_DIRECTIONS, TILE_COLORS):
        cipher[direction] = {"colors": {color: {}}}
    return cipher


def cipher_placement_cost(state: dict, pos: tuple, color: str) -> int:
    existing = get_in(state, ["cipher", pos, "colors"]) or {}
    return 0 if color in existing else len(existing)


def cipher_place_beacon(state: dict, player: str, pos: tuple, color: str, from_bag: bool) -> dict:
    state = deep_copy(state)
    colors = state["cipher"][pos].setdefault("colors", {})
    new_color = color not in colors
    if new_color:
        colors[color] = {}
        if from_bag:
            state["bag"][color] -= 1
    colors[color][player] = colors[color].get(player, 0) + 1
    return state


# ── tile / board ──────────────────────────────────────────────────────────────

def make_tile(color: str) -> dict:
    return {"color": color, "world": True, "station": None, "beacon": None, "sundivers": {}}


def add_station(tile: dict, station_type: str, player: str, level: int) -> dict:
    tile = dict(tile)
    tile["station"] = {"type": station_type, "player": player, "level": level}
    return tile


def get_tile(state: dict, pos: tuple) -> dict | None:
    return state["board"].get(pos)


# ── player ────────────────────────────────────────────────────────────────────

def initial_player() -> dict:
    return {
        "habitat": {"sundivers": 8},
        "movement": MOVEMENT_MIN,
        "stations": {},
        "gates": {},
        "reserve": {
            "sundivers": 5, "foundries": 3, "matrixes": 3, "towers": 3,
            "gates": 8, "beacons": 21, "level_platforms": 13,
        },
        "held_card": None,
    }


# ── initial state ─────────────────────────────────────────────────────────────

def initial_state(turn_order: list[str]) -> dict:
    bag = full_bag()
    bag, first_world = draw_from_bag(bag)
    first_tile = add_station(make_tile(first_world), "tower", "NEUTRAL", 0)
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
        "player_turn": {"player": turn_order[0], "phase": "choose_action_type",
                        "captain_beacons_joined": 0},
        "game_over": None,
    }


# ── accessors ─────────────────────────────────────────────────────────────────

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
    return [ark, hx.add(ark, d), hx.add(ark, hx.rotate_ccw(d)), hx.add(ark, hx.rotate_cw(d))]


# ── immobility ────────────────────────────────────────────────────────────────

def immobile(state: dict, player: str, pos: tuple) -> bool:
    return (player, pos) in (get_in(state, ["player_turn", "immobile"]) or set())


def mark_immobile(state: dict, player: str, pos: tuple) -> dict:
    state = deep_copy(state)
    immobile_set = get_in(state, ["player_turn", "immobile"]) or set()
    state["player_turn"]["immobile"] = set(immobile_set) | {(player, pos)}
    return state


# ── explore / launch / fly ────────────────────────────────────────────────────

def explore(state: dict, player: str, pos: tuple) -> dict:
    bag, color = draw_from_bag(state["bag"])
    tile = make_tile(color)
    tile["sundivers"][player] = 1
    state = deep_copy(state)
    state["bag"] = bag
    state["board"][pos] = tile
    return mark_immobile(state, player, pos)


def launch_sundiver(state: dict, player: str, pos: tuple) -> dict:
    state = deep_copy(state)
    state["players"][player]["habitat"]["sundivers"] -= 1
    if get_tile(state, pos):
        tile = state["board"][pos]
        tile["sundivers"][player] = tile["sundivers"].get(player, 0) + 1
    else:
        state = explore(state, player, pos)
    return state


def gate_owner(state: dict, from_pos: tuple, to_pos: tuple) -> str | None:
    for player in state["turn_order"]:
        gates = (get_in(state, ["players", player, "gates"]) or {})
        if to_pos in gates.get(from_pos, set()):
            return player
    return None


def fly_through_gate(state: dict, player: str, from_pos: tuple, to_pos: tuple) -> dict:
    owner = gate_owner(state, from_pos, to_pos)
    state = deep_copy(state)
    state["board"][from_pos]["sundivers"][player] -= 1
    t = state["board"][to_pos]
    t["sundivers"][player] = t["sundivers"].get(player, 0) + 1
    if owner and owner != player:
        state["players"][owner]["reserve"]["sundivers"] -= 1
        state["players"][owner]["habitat"]["sundivers"] += 1
    return state


def add_gate(state: dict, player: str, from_pos: tuple, to_pos: tuple) -> dict:
    state = deep_copy(state)
    state["board"][from_pos]["sundivers"][player] -= 1
    p = state["players"][player]
    p["reserve"]["sundivers"] = p["reserve"].get("sundivers", 0) + 1
    p["reserve"]["gates"] -= 1
    p.setdefault("gates", {}).setdefault(from_pos, set()).add(to_pos)
    p["gates"].setdefault(to_pos, set()).add(from_pos)
    pt = state["player_turn"].setdefault("action", {})
    pt["gates_created"] = pt.get("gates_created", 0) + 1
    return state


def fly_sundiver(state: dict, player: str, from_pos: tuple, to_pos: tuple) -> dict:
    from_tile = get_tile(state, from_pos)
    to_tile = get_tile(state, to_pos)
    if to_tile is None:
        state = deep_copy(state)
        state["board"][from_pos]["sundivers"][player] -= 1
        return explore(state, player, to_pos)
    elif from_tile["color"] == to_tile["color"]:
        state = deep_copy(state)
        state["board"][from_pos]["sundivers"][player] -= 1
        state["board"][to_pos]["sundivers"][player] = \
            state["board"][to_pos]["sundivers"].get(player, 0) + 1
        return state
    else:
        return add_gate(state, player, from_pos, to_pos)


# ── sundiver spending ─────────────────────────────────────────────────────────

def spend_sundiver(state: dict, player: str, pos: tuple | None) -> dict:
    """Move one sundiver for player from pos (None = habitat) to reserve."""
    state = deep_copy(state)
    if pos is not None:
        state["board"][pos]["sundivers"][player] -= 1
    else:
        state["players"][player]["habitat"]["sundivers"] -= 1
    state["players"][player]["reserve"]["sundivers"] += 1
    return state


def sundiver_spend_positions(state: dict, player: str) -> set:
    """Positions where player has board sundivers, plus None for habitat if non-empty."""
    board_positions = {
        pos for pos, tile in state["board"].items()
        if tile["sundivers"].get(player, 0) > 0
    }
    if state["players"][player]["habitat"]["sundivers"] > 0:
        board_positions.add(None)
    return board_positions


def total_spendable_sundivers(state: dict, player: str) -> int:
    board = sum(t["sundivers"].get(player, 0) for t in state["board"].values())
    hab = state["players"][player]["habitat"]["sundivers"]
    return board + hab


# ── ark movement ──────────────────────────────────────────────────────────────

def turn_heading(state: dict, turn_dir: str) -> dict:
    """Rotate heading :left (CCW) or :right (CW); :none unchanged."""
    if turn_dir == "none":
        return state
    state = deep_copy(state)
    ark = state["ark"]
    d = heading_direction(state)
    new_dir = hx.rotate_ccw(d) if turn_dir == "left" else hx.rotate_cw(d)
    state["heading_token"] = hx.add(ark, new_dir)
    return state


def advance_ark(state: dict) -> dict:
    """Move Ark to heading_token; heading_token advances one more step."""
    state = deep_copy(state)
    d = heading_direction(state)
    new_ark = state["heading_token"]
    state["ark"] = new_ark
    state["heading_token"] = hx.add(new_ark, d)
    return state


def advance_ark_to(state: dict, target_pos: tuple) -> dict:
    """Move Ark to target_pos; heading_token advances one step in current direction."""
    state = deep_copy(state)
    d = heading_direction(state)
    state["ark"] = target_pos
    state["heading_token"] = hx.add(target_pos, d)
    return state


def wrap_target(state: dict, from_pos: tuple, direction: tuple) -> tuple:
    """Target position for wrapping when moving from from_pos in direction.

    Mirrors Clojure wrap-target: finds farthest board tile at distance >= (radius-1)
    along the opposite ray, or falls back to (radius-1) steps in the opposite direction.
    """
    radius = state.get("wrapping_radius", hx.DEFAULT_WRAPPING_RADIUS)
    steps = radius - 1
    dir_idx = hx.direction_index(direction)
    opp_dir = hx.HEX_DIRECTIONS[(dir_idx + 3) % 6]
    board = state["board"]
    far_tiles = [
        pos for pos in board
        if hx.on_ray(from_pos, opp_dir, pos) and hx.distance(from_pos, pos) >= steps
    ]
    if far_tiles:
        return max(far_tiles, key=lambda p: hx.distance(from_pos, p))
    q, r = from_pos
    dq, dr = opp_dir
    return (q + steps * dq, r + steps * dr)


# ── beacons ───────────────────────────────────────────────────────────────────

def place_beacon(state: dict, player: str, pos: tuple) -> dict:
    state = deep_copy(state)
    state["board"][pos]["beacon"] = player
    state["players"][player]["reserve"]["beacons"] -= 1
    return state


def discover_beacon(state: dict, pos: tuple, actor_player: str) -> dict:
    """Remove tile with beacon, enqueue in pending_cipher for end-of-turn resolution."""
    state = deep_copy(state)
    tile = state["board"].pop(pos)
    state["pending_cipher"].append({
        "world": tile["color"],
        "beacon_owner": tile["beacon"],
        "discoverer": actor_player,
        "joiners": [],
    })
    return state


def join_beacon_to_cipher(state: dict, player: str, cipher_idx: int | None = None) -> dict:
    """Add player's beacon to a pending_cipher entry."""
    state = deep_copy(state)
    if cipher_idx is None:
        cipher_idx = len(state["pending_cipher"]) - 1
    if cipher_idx < 0:
        return state
    state["pending_cipher"][cipher_idx]["joiners"].append(player)
    state["players"][player]["reserve"]["beacons"] -= 1
    return state


def matrix_beacon_positions(state: dict, player: str) -> list[tuple]:
    """Valid positions for a matrix beacon: Ark, behind, flanks, or where player has sundivers.
    Must have a world tile with no existing beacon.
    """
    ark = state["ark"]
    d = heading_direction(state)
    dir_idx = hx.direction_index(d)
    behind = hx.add(ark, hx.HEX_DIRECTIONS[(dir_idx + 3) % 6])
    specials = {
        ark, behind,
        hx.add(ark, hx.rotate_ccw(d)),
        hx.add(ark, hx.rotate_cw(d)),
    } | {
        pos for pos, tile in state["board"].items()
        if tile["sundivers"].get(player, 0) > 0
    }
    return [
        pos for pos in specials
        if (t := get_tile(state, pos)) and t.get("world") and t.get("beacon") is None
    ]


# ── region logic ──────────────────────────────────────────────────────────────

def region_tiles(state: dict, pos: tuple) -> set:
    """BFS: all tiles in the same contiguous same-color region as pos."""
    color = state["board"].get(pos, {}).get("color")
    if color is None:
        return set()
    board = state["board"]
    visited = {pos}
    queue = [pos]
    while queue:
        curr = queue.pop(0)
        for n in hx.neighbors(curr):
            if n not in visited and board.get(n, {}).get("color") == color:
                visited.add(n)
                queue.append(n)
    return visited


def region_station_level(state: dict, region: set) -> int | None:
    for pos in region:
        level = get_in(state, ["board", pos, "station", "level"])
        if level is not None:
            return level
    return None


def gates_leaving_region(state: dict, region: set) -> set:
    result = set()
    for player in state["turn_order"]:
        for pos in region:
            gates = get_in(state, ["players", player, "gates"]) or {}
            for dest in gates.get(pos, set()):
                if dest not in region:
                    result.add(dest)
    return result


def region_level(state: dict, target: tuple) -> int:
    region = region_tiles(state, target)
    existing = region_station_level(state, region)
    if existing is not None:
        return existing
    exits = gates_leaving_region(state, region)
    neighbor_levels = [
        region_station_level(state, region_tiles(state, exit))
        for exit in exits
        if region_station_level(state, region_tiles(state, exit)) is not None
    ]
    return max(neighbor_levels) + 1 if neighbor_levels else 1


# ── conversion pattern detection ─────────────────────────────────────────────

def player_adjacent_sundivers(state: dict, player: str, target: tuple) -> list[tuple]:
    """Sundiver positions owned by player that are adjacent to target."""
    return [
        pos for pos in hx.neighbors(target)
        if state["board"].get(pos, {}).get("sundivers", {}).get(player, 0) > 0
        and hx.adjacent_direction(target, pos) is not None
    ]


def find_conversions(state: dict, player: str) -> list[dict]:
    """Find all valid conversion patterns for player.

    Mirrors Clojure find-conversions exactly:
      Foundry: two sundivers adjacent to target at 120° from each other (direction_diff == 2)
      Matrix:  two sundivers adjacent to target directly across (direction_diff == 3)
      Tower:   three sundivers adjacent to target equally spaced (dirs i, i+2, i+4)
    Target tile must exist and have no station.
    """
    board = state["board"]
    conversions = []
    for target in board:
        if board[target].get("station") is not None:
            continue
        adj = player_adjacent_sundivers(state, player, target)
        indexed = [(s, hx.adjacent_direction(target, s)) for s in adj]

        # Foundry: direction_diff == 2
        for i, (s1, d1) in enumerate(indexed):
            for s2, d2 in indexed[i + 1:]:
                if hx.direction_diff(d1, d2) == 2:
                    conversions.append({"type": "foundry", "target": target, "sundivers": [s1, s2]})

        # Matrix: direction_diff == 3
        for i, (s1, d1) in enumerate(indexed):
            for s2, d2 in indexed[i + 1:]:
                if hx.direction_diff(d1, d2) == 3:
                    conversions.append({"type": "matrix", "target": target, "sundivers": [s1, s2]})

        # Tower: three equally spaced (directions i, i+2, i+4)
        for i, (s1, d1) in enumerate(indexed):
            for j, (s2, d2) in enumerate(indexed):
                for s3, d3 in indexed:
                    if s1 != s2 and s1 != s3 and s2 != s3:
                        ds = sorted([d1, d2, d3])
                        if ds in ([0, 2, 4], [1, 3, 5]) and d1 < d2 < d3:
                            conversions.append({"type": "tower", "target": target,
                                                "sundivers": [s1, s2, s3]})
    return conversions


# ── convert ───────────────────────────────────────────────────────────────────

def convert(state: dict, player: str, station_type: str, target: tuple,
            sundiver_positions: list) -> dict:
    """Return sundivers to reserve, place station, then auto-activate once."""
    level = region_level(state, target)
    state = deep_copy(state)
    for pos in sundiver_positions:
        state["board"][pos]["sundivers"][player] -= 1
        state["players"][player]["reserve"]["sundivers"] += 1
    state["board"][target]["station"] = {"type": station_type, "player": player, "level": level}
    state["players"][player]["stations"][target] = {"type": station_type, "level": level}
    state["players"][player]["reserve"][STATION_RESERVE_KEY[station_type]] -= 1
    state["player_turn"].setdefault("action", {})["cards_to_draw"] = level
    return auto_activate_converted_station(state, player, station_type, target)


def auto_activate_converted_station(state: dict, player: str, station_type: str,
                                     target: tuple) -> dict:
    """Trigger one automatic activation of a newly converted station, if feasible."""
    if station_type == "matrix":
        positions = len(matrix_beacon_positions(state, player))
        spendable = total_spendable_sundivers(state, player)
        beacons = get_in(state, ["players", player, "reserve", "beacons"]) or 0
        feasible = positions > 0 and spendable > 0 and beacons > 0
    elif station_type == "tower":
        feasible = total_spendable_sundivers(state, player) > 0
    else:  # foundry
        feasible = True

    if not feasible:
        state = deep_copy(state)
        state["player_turn"]["phase"] = "draw_cards"
        return state

    state = deep_copy(state)
    if station_type == "tower":
        state = become_captain(state, player)
    pt = state["player_turn"]
    pt["action"].update({
        "station_type": station_type,
        "stations_queue": [],
        "current_station": target,
        "current_owner": player,
        "bonus_total": 0,
        "beacons_joined": 0,
        "activator_actions": 1,
        "owner_actions": 0,
    })
    pt["choice_player"] = None
    return begin_actor_actions(state, "activator")


# ── activation machinery ──────────────────────────────────────────────────────

def station_action_counts(level: int) -> dict:
    return ACTIVATION_ACTIONS_TABLE[min(max(level, 0), 3)]


def actor_player(state: dict, actor: str) -> str:
    if actor == "activator":
        return current_player(state)
    return get_in(state, ["player_turn", "action", "current_owner"])


def foundry_action(state: dict, player: str) -> dict:
    """Move up to 2 sundivers from reserve to habitat."""
    available = get_in(state, ["players", player, "reserve", "sundivers"]) or 0
    amount = min(2, available)
    state = deep_copy(state)
    state["players"][player]["reserve"]["sundivers"] -= amount
    state["players"][player]["habitat"]["sundivers"] += amount
    return state


def become_captain(state: dict, player: str) -> dict:
    state = deep_copy(state)
    state["captain_flame"] = player
    return state


def begin_actor_actions(state: dict, actor: str) -> dict:
    """Start the given actor's action sequence."""
    actions_key = "activator_actions" if actor == "activator" else "owner_actions"
    n_actions = get_in(state, ["player_turn", "action", actions_key]) or 0
    station_type = get_in(state, ["player_turn", "action", "station_type"])
    aplayer = actor_player(state, actor)

    if n_actions == 0:
        return advance_after_actions(state, actor)

    if station_type == "foundry":
        s = state
        for _ in range(n_actions):
            s = foundry_action(s, aplayer)
        s = deep_copy(s)
        s["player_turn"]["action"][actions_key] = 0
        return advance_after_actions(s, actor)

    state = deep_copy(state)
    pt = state["player_turn"]
    pt["action"]["current_actor"] = actor
    pt["phase"] = "choose_activate_matrix_beacon" if station_type == "matrix" \
        else "choose_activate_tower_heading"
    return state


def advance_after_actions(state: dict, actor: str) -> dict:
    """After current actor finishes: offer owner bonus or move to next station."""
    if actor == "activator":
        bonus_total = get_in(state, ["player_turn", "action", "bonus_total"]) or 0
        owner = get_in(state, ["player_turn", "action", "current_owner"])
        player = current_player(state)
        if bonus_total > 0 and player != owner:
            state = deep_copy(state)
            state["player_turn"]["choice_player"] = owner
            state["player_turn"]["phase"] = "choose_activate_owner_bonus"
            return state
    return begin_next_station(state)


def begin_next_station(state: dict) -> dict:
    """Pop next station from queue and set it up, or finish the activate action."""
    player = current_player(state)
    queue = get_in(state, ["player_turn", "action", "stations_queue"]) or []
    station_type = get_in(state, ["player_turn", "action", "station_type"])

    if not queue:
        state = deep_copy(state)
        state["player_turn"]["phase"] = "draw_cards"
        return state

    pos = queue[0]
    rest_q = queue[1:]
    tile = get_tile(state, pos)
    owner = get_in(tile, ["station", "player"])
    level = get_in(tile, ["station", "level"]) or 0
    counts = station_action_counts(level)
    base, bonus = counts["base"], counts["bonus"]
    same = player == owner

    state = deep_copy(state)
    if station_type == "tower":
        state = become_captain(state, player)
    pt = state["player_turn"]
    pt["action"].update({
        "stations_queue": rest_q,
        "current_station": pos,
        "current_owner": owner,
        "bonus_total": bonus,
        "beacons_joined": 0,
    })
    pt["action"]["cards_to_draw"] = (pt["action"].get("cards_to_draw") or 0) + level

    if same and bonus > 0:
        pt["action"]["activator_actions"] = base
        pt["action"]["owner_actions"] = 0
        pt["phase"] = "choose_activate_self_bonus"
        return state
    else:
        pt["action"]["activator_actions"] = base
        pt["action"]["owner_actions"] = 0
        return begin_actor_actions(state, "activator")


def stations_of_type_with_sundiver(state: dict, player: str, station_type: str) -> list:
    return [
        pos for pos, tile in state["board"].items()
        if get_in(tile, ["station", "type"]) == station_type
        and tile["sundivers"].get(player, 0) > 0
    ]


def total_activation_actions(state: dict, player: str, station_type: str) -> int:
    return sum(
        station_action_counts(get_in(state, ["board", pos, "station", "level"]) or 0)["base"]
        for pos in stations_of_type_with_sundiver(state, player, station_type)
    )


def can_fully_activate_matrix(state: dict, player: str) -> bool:
    n = total_activation_actions(state, player, "matrix")
    positions = len(matrix_beacon_positions(state, player))
    spendable = total_spendable_sundivers(state, player)
    beacons = get_in(state, ["players", player, "reserve", "beacons"]) or 0
    return n == 0 or (positions >= n and spendable >= n and beacons >= n)


def can_fully_activate_tower(state: dict, player: str) -> bool:
    n = total_activation_actions(state, player, "tower")
    return n == 0 or total_spendable_sundivers(state, player) >= n


def can_fully_activate_foundry(state: dict, player: str) -> bool:
    n = total_activation_actions(state, player, "foundry")
    reserve = get_in(state, ["players", player, "reserve", "sundivers"]) or 0
    return n == 0 or reserve >= n


def activatable_station_types(state: dict, player: str) -> set:
    result = set()
    for pos, tile in state["board"].items():
        stype = get_in(tile, ["station", "type"])
        if stype and tile["sundivers"].get(player, 0) > 0:
            ok = (
                can_fully_activate_matrix(state, player) if stype == "matrix" else
                can_fully_activate_tower(state, player) if stype == "tower" else
                can_fully_activate_foundry(state, player)
            )
            if ok:
                result.add(stype)
    return result


def start_activate(state: dict, station_type: str) -> dict:
    player = current_player(state)
    stations = stations_of_type_with_sundiver(state, player, station_type)
    state = deep_copy(state)
    state["player_turn"]["action"] = {
        **state["player_turn"].get("action", {}),
        "station_type": station_type,
        "stations_queue": list(stations),
    }
    return begin_next_station(state)


# ── choose action type ────────────────────────────────────────────────────────

def choose_action_type(state: dict, action_type: str) -> dict:
    player = current_player(state)
    action_data = {"moves_remaining": move_points(state, player)} if action_type == "move" else {}
    state = deep_copy(state)
    state["player_turn"]["action_type"] = action_type
    state["player_turn"]["action"] = action_data
    state["player_turn"]["phase"] = f"choose_{action_type}"
    return state


# ── movement points ───────────────────────────────────────────────────────────

def station_colors(state: dict, player: str) -> set:
    """Distinct tile colors where player has stations, plus neutral tower tile color."""
    player_positions = list((get_in(state, ["players", player, "stations"]) or {}).keys())
    neutral_pos = state.get("neutral_tower")
    all_positions = player_positions + ([neutral_pos] if neutral_pos else [])
    return {state["board"][pos]["color"] for pos in all_positions if pos in state["board"]}


def move_points(state: dict, player: str) -> int:
    """Base 2 + 1 per distinct tile color player has a station on (including neutral tower)."""
    return 2 + len(station_colors(state, player))


# ── card draw / flare processing ──────────────────────────────────────────────

def compute_draw_count(state: dict) -> int:
    action_type = get_in(state, ["player_turn", "action_type"])
    action = get_in(state, ["player_turn", "action"]) or {}
    if action_type == "move":
        return action.get("gates_created", 0)
    return action.get("cards_to_draw", 0)


def advance_flare_ark_to(state: dict, new_ark: tuple) -> dict:
    """Advance Ark to new_ark for one flare; handle beacon; continue flare processing."""
    player = current_player(state)
    captain = state["captain_flame"]
    tile = get_tile(state, new_ark)
    state = advance_ark_to(state, new_ark)
    state = deep_copy(state)
    state["player_turn"]["pending_flares"] -= 1

    if tile and tile.get("beacon"):
        beacon_owner = tile["beacon"]
        cipher_idx = len(state["pending_cipher"])
        state = discover_beacon(state, new_ark, player)
        if beacon_owner != captain:
            indices = get_in(state, ["player_turn", "action", "flare_join_indices"]) or []
            state["player_turn"]["action"]["flare_join_indices"] = indices + [cipher_idx]

    return continue_flare_processing(state)


def continue_flare_processing(state: dict) -> dict:
    """Process the next pending flare advance."""
    remaining = get_in(state, ["player_turn", "pending_flares"]) or 0
    if remaining == 0:
        return after_all_flares(state)
    next_pos = state["heading_token"]
    if get_tile(state, next_pos):
        return advance_flare_ark_to(state, next_pos)
    state = deep_copy(state)
    state["player_turn"]["phase"] = "choose_flare_advance"
    return state


def after_all_flares(state: dict) -> dict:
    non_flares = get_in(state, ["player_turn", "pending_non_flares"]) or []
    join_indices = get_in(state, ["player_turn", "action", "flare_join_indices"]) or []
    captain = state["captain_flame"]
    state = deep_copy(state)
    pt = state["player_turn"]
    pt.pop("pending_flares", None)
    pt.pop("pending_non_flares", None)
    pt["action"]["drawn_cards"] = list(non_flares)
    if join_indices:
        pt["choice_player"] = captain
        pt["phase"] = "flare_beacon_join"
    else:
        pt["phase"] = "keep_card"
    return state


def process_draw_and_flares(state: dict) -> dict:
    """Draw cards; advance Ark per flare; queue beacon joins."""
    n = compute_draw_count(state)
    state, cards = draw_n_cards(state, n)
    flares = [c for c in cards if flare(c)]
    non_flares = [c for c in cards if not flare(c)]
    state = deep_copy(state)
    state["flares_drawn"] = (state.get("flares_drawn") or 0) + len(flares)
    if state["flares_drawn"] >= 13:
        return trigger_loss(state)
    state["player_turn"]["pending_flares"] = len(flares)
    state["player_turn"]["pending_non_flares"] = non_flares
    state["player_turn"].setdefault("action", {})["flare_join_indices"] = []
    state["player_turn"]["captain_beacons_joined"] = 0
    return continue_flare_processing(state)


# ── landing ───────────────────────────────────────────────────────────────────

def count_cipher_matches(state: dict, pos: tuple) -> int:
    count = 0
    for direction in hx.HEX_DIRECTIONS:
        neighbor = hx.add(pos, direction)
        neighbor_color = state["board"].get(neighbor, {}).get("color")
        cipher_colors = get_in(state, ["cipher", direction, "colors"]) or {}
        if neighbor_color and neighbor_color in cipher_colors:
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


def available_landings(state: dict) -> list[tuple]:
    return [pos for pos in state["board"] if can_land_at(state, pos)]


def compute_scores(state: dict, pos: tuple) -> dict[str, int]:
    landed_color = state["board"].get(pos, {}).get("color")
    scores: dict[str, int] = {}

    def add_beacons(beacons: dict):
        for p, n in beacons.items():
            scores[p] = scores.get(p, 0) + n

    add_beacons(get_in(state, ["cipher", (0, 0), "colors", landed_color]) or {})
    for direction in hx.HEX_DIRECTIONS:
        neighbor = hx.add(pos, direction)
        neighbor_color = state["board"].get(neighbor, {}).get("color")
        if neighbor_color:
            add_beacons(get_in(state, ["cipher", direction, "colors", neighbor_color]) or {})
    return scores


def land_ark(state: dict, pos: tuple) -> dict:
    scores = compute_scores(state, pos)
    max_score = max(scores.values()) if scores else 0
    winners = [p for p in state["turn_order"] if scores.get(p, 0) == max_score]
    state = deep_copy(state)
    state["game_over"] = {"type": "landing", "tile": pos, "scores": scores, "winners": winners}
    state["player_turn"]["phase"] = "game_over"
    return state


def trigger_loss(state: dict) -> dict:
    state = deep_copy(state)
    state["game_over"] = {"type": "loss", "captain": state["captain_flame"]}
    state["player_turn"]["phase"] = "game_over"
    return state


# ── turn transition ───────────────────────────────────────────────────────────

def begin_next_player_turn(state: dict) -> dict:
    order = state["turn_order"]
    cur = current_player(state)
    idx = order.index(cur)
    next_idx = (idx + 1) % len(order)
    next_p = order[next_idx]
    new_round = (state.get("round") or 0) + (1 if next_idx == 0 else 0)
    state = deep_copy(state)
    state["round"] = new_round
    state["pending_cipher"] = []
    state["player_turn"] = {
        "player": next_p,
        "phase": "choose_action_type",
        "captain_beacons_joined": 0,
    }
    return state
