"""Organism game state functions.

All functions take and return the game dict (which contains both static
board data and dynamic state). State is copied before mutation to support
MCTS tree search.

game dict structure:
  {
    "rings":          list[list[space]]
    "adjacencies":    dict[space, list[space]]
    "center":         space
    "capture_limit":  int
    "players":        dict[player, {"starting_spaces": list[space]}]
    "turn_order":     list[player]
    "organism_victory": int
    "space_to_idx":   dict[space, int]   (for encoding)
    "idx_to_space":   list[space]         (for encoding)
    "state": {
      "round":       int
      "elements":    dict[space, element]
      "food":        dict[space, int]
      "captures":    dict[player, list]
      "player_turn": {
        "player":        player
        "introduction":  dict
        "organism_turns": list[organism_turn]
        "advance":       str | None
      }
      "winner":      player | None
    }
  }

element dict:
  {"player": str, "organism": int|None, "type": str, "space": space, "food": int, "captures": list}

organism_turn dict:
  {"organism": int, "choice": str|None, "num_actions": int, "actions": list[action]}

action dict:
  {"type": str, "action": dict}
"""

from __future__ import annotations
import copy
from typing import Any

Space = tuple[int, int]
Game = dict


# ── helpers ────────────────────────────────────────────────────────────────────

def _deep(game: Game) -> Game:
    return copy.deepcopy(game)


def get_state(game: Game) -> dict:
    return game["state"]


def adjacent_to(game: Game, space: Space) -> list[Space]:
    return game["adjacencies"].get(space, [])


def get_element(game: Game, space: Space) -> dict | None:
    return game["state"]["elements"].get(space)


def element_spaces(game: Game) -> list[Space]:
    return list(game["state"]["elements"].keys())


def free_food_present(game: Game, space: Space) -> int:
    return game["state"]["food"].get(space, 0)


# ── element mutation ────────────────────────────────────────────────────────────

def add_element(game: Game, player: str, organism: int | None, etype: str, space: Space, food: int) -> Game:
    game = _deep(game)
    game["state"]["elements"][space] = {
        "player": player, "organism": organism,
        "type": etype, "space": space,
        "food": food, "captures": [],
    }
    return game


def remove_element(game: Game, space: Space) -> Game:
    game = _deep(game)
    game["state"]["elements"].pop(space, None)
    return game


def remove_free_food(game: Game, space: Space) -> Game:
    game = _deep(game)
    game["state"]["food"].pop(space, None)
    return game


def drop_free_food(game: Game, space: Space, food: int) -> Game:
    game = _deep(game)
    current = game["state"]["food"].get(space, 0)
    game["state"]["food"][space] = current + food
    return game


def claim_free_food(game: Game, space: Space) -> tuple[Game, int]:
    food = free_food_present(game, space)
    if food == 0:
        return game, 0
    return remove_free_food(game, space), food


def adjust_food(game: Game, space: Space, amount: int) -> Game:
    game = _deep(game)
    el = game["state"]["elements"].get(space)
    if el:
        el["food"] = el["food"] + amount
    return game


def deconstruct_element(game: Game, space: Space) -> Game:
    """Remove element, drop its food (food+1) to the space."""
    el = get_element(game, space)
    dropped = 1 + (el["food"] if el else 0)
    game = remove_element(game, space)
    game = drop_free_food(game, space, dropped)
    return game


def lose_element(game: Game, space: Space) -> Game:
    return deconstruct_element(game, space)


def clear_space(game: Game, space: Space) -> Game:
    game = remove_element(game, space)
    game = remove_free_food(game, space)
    return game


def clear_spaces(game: Game, spaces: list[Space]) -> Game:
    for space in spaces:
        game = clear_space(game, space)
    return game


# ── queries ─────────────────────────────────────────────────────────────────────

def adjacent_elements(game: Game, space: Space) -> list[dict]:
    result = []
    for adj in adjacent_to(game, space):
        el = get_element(game, adj)
        if el:
            result.append(el)
    return result


def open_spaces(game: Game, space: Space) -> list[Space]:
    """Empty spaces adjacent to space."""
    return [adj for adj in adjacent_to(game, space) if not get_element(game, adj)]


def available_spaces(game: Game, space: Space) -> list[Space]:
    """Empty adjacent spaces where moving is legal (no same-type enemy adjacent)."""
    el = get_element(game, space)
    if not el:
        return []
    result = []
    for adj in open_spaces(game, space):
        adj_els = adjacent_elements(game, adj)
        blocked = any(
            other["type"] == el["type"] and other["player"] != el["player"]
            for other in adj_els
        )
        if not blocked:
            result.append(adj)
    return result


def growable_adjacent(game: Game, space: Space) -> list[Space]:
    """Empty adjacent spaces where growing is legal (no enemy adjacent)."""
    el = get_element(game, space)
    if not el:
        return []
    result = []
    for adj in open_spaces(game, space):
        adj_els = adjacent_elements(game, adj)
        blocked = any(other["player"] != el["player"] for other in adj_els)
        if not blocked:
            result.append(adj)
    return result


def growable_spaces(game: Game, spaces: list[Space]) -> set[Space]:
    result: set[Space] = set()
    for space in spaces:
        result.update(growable_adjacent(game, space))
    return result


def surrounding_spaces(game: Game, spaces: list[Space]) -> set[Space]:
    result: set[Space] = set()
    for space in spaces:
        result.update(adjacent_to(game, space))
    return result


def fed_element(element: dict) -> bool:
    return element["food"] > 0


def fed(game: Game, space: Space) -> bool:
    el = get_element(game, space)
    return el is not None and el["food"] > 0


def friendly_adjacent_elements(game: Game, space: Space) -> list[dict]:
    el = get_element(game, space)
    if not el:
        return []
    return [
        other for other in adjacent_elements(game, space)
        if other["player"] == el["player"]
    ]


def mobile(game: Game, space: Space) -> bool:
    el = get_element(game, space)
    if not el:
        return False
    orbit = [el] + friendly_adjacent_elements(game, space)
    return any(o["type"] == "move" for o in orbit)


def alive_elements(elements: list[dict]) -> bool:
    return len({e["type"] for e in elements}) >= 3


def contiguous_elements(game: Game, space: Space) -> list[Space]:
    el = get_element(game, space)
    if not el:
        return []
    player = el["player"]
    visited: set[Space] = set()
    queue = [space]
    result: list[Space] = []
    while queue:
        cur = queue.pop(0)
        if cur in visited:
            continue
        visited.add(cur)
        cur_el = get_element(game, cur)
        if not cur_el:
            continue
        if cur_el["player"] == player:
            result.append(cur)
            for adj in adjacent_to(game, cur):
                if adj not in visited:
                    adj_el = get_element(game, adj)
                    if adj_el and adj_el["player"] == player:
                        queue.append(adj)
    return result


def alive(game: Game, space: Space) -> bool:
    contiguous = contiguous_elements(game, space)
    elements = [get_element(game, s) for s in contiguous]
    return alive_elements([e for e in elements if e])


def can_move(game: Game, space: Space) -> bool:
    return fed(game, space) and mobile(game, space) and alive(game, space)


def can_eat(game: Game, element: dict) -> bool:
    return len(open_spaces(game, element["space"])) > 0


def open_element(element: dict) -> bool:
    return element["food"] < 111


# ── player / organism queries ───────────────────────────────────────────────────

def player_elements(game: Game) -> dict[str, list[dict]]:
    result: dict[str, list[dict]] = {}
    for el in game["state"]["elements"].values():
        result.setdefault(el["player"], []).append(el)
    return result


def player_organisms(game: Game, player: str) -> dict[int, list[dict]]:
    """Map organism_id → [elements] for a given player."""
    result: dict[int, list[dict]] = {}
    for el in game["state"]["elements"].values():
        if el["player"] == player:
            result.setdefault(el["organism"], []).append(el)
    return result


# ── organism numbering ──────────────────────────────────────────────────────────

def _clear_organisms(game: Game) -> Game:
    game = _deep(game)
    for el in game["state"]["elements"].values():
        el["organism"] = None
    return game


def _set_organism(game: Game, space: Space, org_id: int) -> Game:
    el = game["state"]["elements"].get(space)
    if el:
        el["organism"] = org_id
    return game


def _trace_organism(game: Game, start: Space, org_id: int) -> Game:
    spaces = contiguous_elements(game, start)
    for space in spaces:
        el = game["state"]["elements"].get(space)
        if el:
            el["organism"] = org_id
    return game


def find_organisms(game: Game) -> Game:
    """Re-label all elements with contiguous organism IDs (modifies in place after deepcopy)."""
    game = _clear_organisms(game)
    org_id = 0
    for space, el in list(game["state"]["elements"].items()):
        if el["organism"] is None:
            game = _trace_organism(game, space, org_id)
            org_id += 1
    return game


def group_organisms(game: Game) -> dict[int, list[dict]]:
    result: dict[int, list[dict]] = {}
    for el in game["state"]["elements"].values():
        result.setdefault(el["organism"], []).append(el)
    return result


# ── introduction ────────────────────────────────────────────────────────────────

def introduce_spaces(game: Game, player: str, introduction: dict) -> Game:
    """Place elements on starting spaces. introduction = {"organism": 0, "spaces": {space: type}}."""
    starting = game["players"][player]["starting_spaces"]
    surround = surrounding_spaces(game, starting)
    game = clear_spaces(game, list(surround))
    organism = introduction.get("organism", 0)
    for space, etype in introduction["spaces"].items():
        game = add_element(game, player, organism, etype, space, 1)
    game = _deep(game)
    game["state"]["player_turn"]["introduction"] = introduction
    return game


# ── action execution ────────────────────────────────────────────────────────────

def eat_action(game: Game, fields: dict) -> Game:
    """eat: {to: space, from: space}"""
    from_space = fields["from"]
    to_space = fields["to"]
    amount = 1 + free_food_present(game, from_space)
    game = remove_free_food(game, from_space)
    game = adjust_food(game, to_space, amount)
    return game


def grow_action(game: Game, fields: dict) -> Game:
    """grow: {element: type, from: {space: food_spent}, to: space}"""
    player = game["state"]["player_turn"]["player"]
    org_turn = game["state"]["player_turn"]["organism_turns"][-1]
    organism = org_turn["organism"]
    etype = fields["element"]
    from_dict = fields["from"]
    to_space = fields["to"]

    # Deduct food from contributing growers
    for space, food_spent in from_dict.items():
        game = adjust_food(game, space, -food_spent)

    # Claim any free food on destination
    game, free = claim_free_food(game, to_space)
    game = add_element(game, player, organism, etype, to_space, free)
    return game


def move_action(game: Game, fields: dict) -> Game:
    """move: {from: space, to: space}"""
    from_space = fields["from"]
    to_space = fields["to"]
    el = get_element(game, from_space)
    if not el:
        return game
    game = remove_element(game, from_space)
    game = _deep(game)
    moved = dict(el)
    moved["space"] = to_space
    game["state"]["elements"][to_space] = moved
    game, free = claim_free_food(game, to_space)
    game = adjust_food(game, to_space, free)
    return game


def circulate_action(game: Game, fields: dict) -> Game:
    """circulate: {from: space, to: space}"""
    game = adjust_food(game, fields["from"], -1)
    game = adjust_food(game, fields["to"], 1)
    return game


ACTION_MAP = {
    "eat": eat_action,
    "grow": grow_action,
    "move": move_action,
    "circulate": circulate_action,
}

ACTION_FIELDS = {
    "eat": ["to", "from"],
    "grow": ["element", "from", "to"],
    "move": ["from", "to"],
    "circulate": ["from", "to"],
}


def complete_action(action_entry: dict) -> bool:
    """True if all required fields are present or action is a pass."""
    atype = action_entry["type"]
    fields = action_entry["action"]
    if fields.get("pass"):
        return True
    return all(f in fields for f in ACTION_FIELDS[atype])


def perform_action(game: Game, action_entry: dict) -> Game:
    if action_entry["action"].get("pass"):
        return game
    fn = ACTION_MAP.get(action_entry["type"])
    if fn:
        return fn(game, action_entry["action"])
    return game


# ── organism-turn helpers ────────────────────────────────────────────────────────

def update_organism_turn(game: Game, fn) -> Game:
    game = _deep(game)
    turns = game["state"]["player_turn"]["organism_turns"]
    if turns:
        turns[-1] = fn(turns[-1])
    return game


def choose_organism_action(game: Game, organism: int) -> Game:
    game = _deep(game)
    game["state"]["player_turn"]["organism_turns"].append({
        "organism": organism,
        "choice": None,
        "num_actions": -1,
        "actions": [],
    })
    return game


def choose_action_type_action(game: Game, atype: str) -> Game:
    player = game["state"]["player_turn"]["player"]
    organisms = player_organisms(game, player)

    def _update(turn: dict) -> dict:
        org_elements = organisms.get(turn["organism"], [])
        by_type = {}
        for el in org_elements:
            by_type.setdefault(el["type"], []).append(el)
        num_actions = len(by_type.get(atype, []))
        return {**turn, "choice": atype, "num_actions": num_actions}

    return update_organism_turn(game, _update)


def choose_action_action(game: Game, atype: str) -> Game:
    """Append a new empty action of the given type to the current organism turn."""
    def _update(turn: dict) -> dict:
        return {**turn, "actions": turn["actions"] + [{"type": atype, "action": {}}]}
    return update_organism_turn(game, _update)


def set_action_field(game: Game, field: str, value: Any) -> Game:
    """Set a field on the last action of the current organism turn. Executes action when complete."""
    def _update(turn: dict) -> dict:
        actions = list(turn["actions"])
        if not actions:
            return turn
        last = dict(actions[-1])
        last["action"] = {**last["action"], field: value}
        actions[-1] = last
        return {**turn, "actions": actions}

    game = update_organism_turn(game, _update)

    # If the last action is now complete, execute it
    org_turns = game["state"]["player_turn"]["organism_turns"]
    if org_turns:
        last_action = org_turns[-1]["actions"][-1] if org_turns[-1]["actions"] else None
        if last_action and complete_action(last_action):
            game = perform_action(game, last_action)

    return game


def pass_action(game: Game) -> Game:
    def _update(turn: dict) -> dict:
        actions = list(turn["actions"])
        if not actions:
            return turn
        last = dict(actions[-1])
        last["action"] = {**last["action"], "pass": True}
        actions[-1] = last
        return {**turn, "actions": actions}
    return update_organism_turn(game, _update)


# ── conflict resolution ─────────────────────────────────────────────────────────

HETERARCHY = {"eat": "grow", "grow": "move", "move": "eat"}


def _heterarchy_sort(a: dict, b: dict) -> tuple[dict, dict]:
    """Returns (winner, loser)."""
    if HETERARCHY.get(a["type"]) == b["type"]:
        return a, b
    return b, a


def _element_conflicts(game: Game, element: dict) -> list[tuple[dict, dict]]:
    conflicts = []
    for adj_space in adjacent_to(game, element["space"]):
        other = get_element(game, adj_space)
        if other and other["player"] != element["player"]:
            conflicts.append(_heterarchy_sort(element, other))
    return conflicts


def _player_conflicts(game: Game, player: str) -> list[tuple[dict, dict]]:
    conflicts = []
    for el in game["state"]["elements"].values():
        if el["player"] == player:
            conflicts.extend(_element_conflicts(game, el))
    return conflicts


def _award_capture(game: Game, player: str, captured_el: dict) -> Game:
    game = _deep(game)
    game["state"]["captures"][player].append(captured_el)
    return game


def _resolve_one_conflict(game: Game, rise: dict, fall: dict) -> Game:
    if rise["type"] == fall["type"]:
        # Annihilation
        game = lose_element(game, rise["space"])
        game = lose_element(game, fall["space"])
        game = _award_capture(game, fall["player"], rise)
    else:
        game = lose_element(game, fall["space"])
        game = _award_capture(game, rise["player"], fall)
    return game


def resolve_conflicts(game: Game, player: str) -> Game:
    conflicts = _player_conflicts(game, player)
    # Process annihilations first
    annihilations = [c for c in conflicts if c[0]["type"] == c[1]["type"]]
    others = [c for c in conflicts if c[0]["type"] != c[1]["type"]]

    for rise, fall in annihilations:
        # Re-fetch in case prior conflict removed element
        rise_el = get_element(game, rise["space"])
        fall_el = get_element(game, fall["space"])
        if rise_el and fall_el:
            game = _resolve_one_conflict(game, rise_el, fall_el)

    # Process heterarchy conflicts (in stable topological order)
    processed: set[Space] = set()
    for rise, fall in others:
        if fall["space"] in processed or rise["space"] in processed:
            continue
        rise_el = get_element(game, rise["space"])
        fall_el = get_element(game, fall["space"])
        if rise_el and fall_el:
            game = _resolve_one_conflict(game, rise_el, fall_el)
            processed.add(fall["space"])

    game = _deep(game)
    game["state"]["player_turn"]["advance"] = "resolve_conflicts"
    return game


# ── integrity check ─────────────────────────────────────────────────────────────

def check_integrity(game: Game, active_player: str) -> Game:
    game = find_organisms(game)
    organisms = group_organisms(game)

    for org_id, elements in list(organisms.items()):
        if alive_elements(elements):
            continue
        # Dead organism: remove elements, award capture to active player
        # (only for non-active-player organisms)
        org_players = {el["player"] for el in elements}
        for space in [el["space"] for el in elements]:
            game = lose_element(game, space)
        if active_player not in org_players:
            fake_el = {"type": "integrity", "player": list(org_players)[0]}
            game = _award_capture(game, active_player, fake_el)

    game = _deep(game)
    game["state"]["player_turn"]["advance"] = "check_integrity"
    return game


# ── victory ─────────────────────────────────────────────────────────────────────

def enough_captures(game: Game, player: str) -> bool:
    limit = game["players"][player].get("capture_limit", 5)
    return len(game["state"]["captures"].get(player, [])) >= limit


def living_organism_count(game: Game, player: str) -> int:
    orgs = player_organisms(game, player)
    return sum(1 for els in orgs.values() if alive_elements(els))


def player_wins(game: Game, player: str) -> bool:
    return enough_captures(game, player) or (
        living_organism_count(game, player) >= game["organism_victory"]
    )


def victory(game: Game) -> str | None:
    """Return the winning player name, or None."""
    winners = [p for p in game["turn_order"] if player_wins(game, p)]
    if not winners:
        return None
    if len(winners) == 1:
        return winners[0]
    # Tiebreak: most captures, then most living organisms
    def _score(p):
        return (len(game["state"]["captures"].get(p, [])),
                living_organism_count(game, p))
    return max(winners, key=_score)


# ── turn flow ────────────────────────────────────────────────────────────────────

def award_center(game: Game, player: str) -> Game:
    center = game["center"]
    el = get_element(game, center)
    if el and el["player"] == player:
        fake_el = {"type": "center", "player": player, "space": center}
        game = _award_capture(game, player, fake_el)
    return game


def start_turn(game: Game, player: str) -> Game:
    game = _deep(game)
    game["state"]["player_turn"] = {
        "player": player,
        "introduction": {},
        "organism_turns": [],
        "advance": None,
    }
    game = award_center(game, player)
    return game


def next_player_info(game: Game) -> tuple[int, str]:
    turn_order = game["turn_order"]
    player = game["state"]["player_turn"]["player"]
    idx = turn_order.index(player)
    next_idx = (idx + 1) % len(turn_order)
    return next_idx, turn_order[next_idx]


def start_next_turn(game: Game) -> Game:
    next_idx, next_p = next_player_info(game)
    game = start_turn(game, next_p)
    if next_idx == 0:
        game = _deep(game)
        game["state"]["round"] += 1
    return game


# ── initial state ────────────────────────────────────────────────────────────────

def initial_state(turn_order: list[str]) -> dict:
    return {
        "round": 0,
        "elements": {},
        "food": {},
        "captures": {p: [] for p in turn_order},
        "player_turn": {
            "player": turn_order[0],
            "introduction": {},
            "organism_turns": [],
            "advance": None,
        },
        "winner": None,
    }


def initial_game(
    rings: list,
    adjacencies: dict,
    center,
    player_info: list[tuple[str, dict]],
    organism_victory: int,
    space_to_idx: dict,
    idx_to_space: list,
) -> Game:
    capture_limit = 5
    players = {p: {**info, "capture_limit": capture_limit} for p, info in player_info}
    turn_order = [p for p, _ in player_info]
    return {
        "rings": rings,
        "adjacencies": adjacencies,
        "center": center,
        "capture_limit": capture_limit,
        "players": players,
        "turn_order": turn_order,
        "organism_victory": organism_victory,
        "space_to_idx": space_to_idx,
        "idx_to_space": idx_to_space,
        "state": initial_state(turn_order),
    }


# ── grow-from food contribution enumeration ─────────────────────────────────────

def _extend_contribution(growers: list[dict], contribution: dict) -> list[dict]:
    """Add one more unit of food to an existing partial contribution."""
    result = []
    for el in growers:
        space = el["space"]
        spent = contribution.get(space, 0)
        if el["food"] - spent > 0:
            new_contrib = {**contribution, space: spent + 1}
            result.append(new_contrib)
    return result


def food_contributions(growers: list[dict], total: int) -> list[dict]:
    """All distinct ways to spend `total` food from growers."""
    contributions = [{}]
    for _ in range(total):
        next_contribs = []
        seen = set()
        for c in contributions:
            for nc in _extend_contribution(growers, c):
                key = tuple(sorted(nc.items()))
                if key not in seen:
                    seen.add(key)
                    next_contribs.append(nc)
        contributions = next_contribs
    return contributions
