"""Organism choice enumeration.

find_state(game) → (phase_name: str, choices: dict[action_index: int, next_game: Game])

Action index layout (encoding.py defines the constants):
  0 .. N-1          : board space selection (for single-space choices)
  N+0 .. N+5        : introduce permutation (6 permutations of eat/grow/move)
  N+6               : action type "eat"
  N+7               : action type "grow"
  N+8               : action type "move"
  N+9               : action type "circulate"
  N+10              : grow element "eat"
  N+11              : grow element "grow"
  N+12              : grow element "move"
  N+13              : pass
  N+14 .. N+23      : grow-from contribution index (up to 10 distinct dicts)
  N+24 .. N+33      : choose-organism by organism ID (IDs 0-9)
"""

from __future__ import annotations
from itertools import permutations
from typing import Any

import alphazero.games.organism.state as gs

Game = dict

# Element types in canonical order
ELEMENT_TYPES = ["eat", "grow", "move"]

# All 6 permutations of element types (fixed order for stable action indices)
INTRO_PERMS: list[tuple[str, ...]] = list(permutations(ELEMENT_TYPES))


# ── action index offsets (must match encoding.py) ──────────────────────────────

# These are set at runtime by encoding.py after knowing N.
# Here we use symbolic names; encoding.py imports and sets _N.
_N: int = 0   # set by encoding.py


def _space_idx(game: Game, space) -> int:
    return game["space_to_idx"][space]


def _type_idx(atype: str) -> int:
    return _N + 6 + ["eat", "grow", "move", "circulate"].index(atype)


def _elem_idx(etype: str) -> int:
    return _N + 10 + ELEMENT_TYPES.index(etype)


def _intro_idx(perm: tuple) -> int:
    return _N + INTRO_PERMS.index(perm)


_PASS_IDX = lambda: _N + 13
_GROW_FROM_BASE = lambda: _N + 14
_ORG_BASE = lambda: _N + 24


# ── introduce phase ─────────────────────────────────────────────────────────────

def _introduce_choices(game: Game, player: str) -> dict[int, Game]:
    starting = game["players"][player]["starting_spaces"]
    organism = 0
    choices: dict[int, Game] = {}
    for perm in INTRO_PERMS:
        spaces = {s: t for s, t in zip(starting, perm)}
        introduction = {"organism": organism, "spaces": spaces}
        next_game = gs.introduce_spaces(game, player, introduction)
        idx = _intro_idx(perm)
        choices[idx] = next_game
    return choices


# ── choose-organism phase ───────────────────────────────────────────────────────

def _choose_organism_choices(game: Game, organism_ids: list[int]) -> dict[int, Game]:
    choices: dict[int, Game] = {}
    for org_id in organism_ids:
        next_game = gs.choose_organism_action(game, org_id)
        idx = _ORG_BASE() + (org_id % 10)
        choices[idx] = next_game
    return choices


# ── choose-action-type phase ────────────────────────────────────────────────────

def _action_type_feasible(game: Game, atype: str) -> bool:
    """Check whether the current organism can meaningfully perform this action type."""
    player = game["state"]["player_turn"]["player"]
    org_turn = game["state"]["player_turn"]["organism_turns"][-1]
    organism = org_turn["organism"]
    organisms = gs.player_organisms(game, player)
    elements = organisms.get(organism, [])
    by_type = {t: [e for e in elements if e["type"] == t] for t in ELEMENT_TYPES}

    if atype == "eat":
        eaters = by_type.get("eat", [])
        return any(gs.can_eat(game, e) for e in eaters)
    elif atype == "grow":
        growers = by_type.get("grow", [])
        food = sum(e["food"] for e in growers)
        growable = gs.growable_spaces(game, [e["space"] for e in growers])
        all_counts = [len(v) for v in by_type.values()]
        least = min(all_counts) if len(by_type) >= 3 else 0
        return food >= least and len(growable) > 0
    elif atype == "move":
        return any(gs.can_move(game, e["space"]) for e in elements)
    return False


def _choose_action_type_choices(game: Game) -> dict[int, Game]:
    choices: dict[int, Game] = {}
    for atype in ELEMENT_TYPES:
        next_game = gs.choose_action_type_action(game, atype)
        idx = _type_idx(atype)
        choices[idx] = next_game
    return choices


# ── choose-action phase (type or circulate) ─────────────────────────────────────

def _circulate_feasible(game: Game) -> bool:
    player = game["state"]["player_turn"]["player"]
    org_turn = game["state"]["player_turn"]["organism_turns"][-1]
    organism = org_turn["organism"]
    organisms = gs.player_organisms(game, player)
    elements = organisms.get(organism, [])
    return any(gs.fed_element(e) for e in elements)


def _choose_action_choices(game: Game, action_type: str) -> dict[int, Game]:
    """Player picks to do action_type OR circulate for this slot."""
    choices: dict[int, Game] = {}

    # Option: do the chosen action type
    next_game = gs.choose_action_action(game, action_type)
    choices[_type_idx(action_type)] = next_game

    # Option: circulate instead (if feasible)
    if _circulate_feasible(game):
        circ_game = gs.choose_action_action(game, "circulate")
        choices[_type_idx("circulate")] = circ_game

    # Pass (circulate as pass)
    pass_game = gs.choose_action_action(game, "circulate")
    pass_game = gs.pass_action(pass_game)
    choices[_PASS_IDX()] = pass_game

    return choices


# ── action field choices ────────────────────────────────────────────────────────

def _next_field(action_type: str, action_data: dict) -> str | None:
    for field in gs.ACTION_FIELDS[action_type]:
        if field not in action_data:
            return field
    return None


def _eat_to_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    choices: dict[int, Game] = {}
    eaters = [e for e in elements if e["type"] == "eat" and gs.can_eat(game, e)]
    for eater in eaters:
        next_game = gs.set_action_field(game, "to", eater["space"])
        choices[_space_idx(game, eater["space"])] = next_game
    return choices


def _eat_from_choices(game: Game) -> dict[int, Game]:
    org_turns = game["state"]["player_turn"]["organism_turns"]
    last_action = org_turns[-1]["actions"][-1]["action"]
    to_space = last_action.get("to")
    if not to_space:
        return {}
    # Adjacent empty spaces (where food comes from)
    open_adjs = gs.open_spaces(game, to_space)
    # Deduplicate by whether free food is present (prefer food-present)
    by_food: dict[int, Any] = {}
    choices: dict[int, Game] = {}
    for space in open_adjs:
        food_key = 1 if gs.free_food_present(game, space) > 0 else 0
        if food_key not in by_food:
            by_food[food_key] = space
    for space in by_food.values():
        next_game = gs.set_action_field(game, "from", space)
        choices[_space_idx(game, space)] = next_game
    return choices


def _grow_element_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    by_type = {t: [e for e in elements if e["type"] == t] for t in ELEMENT_TYPES}
    grower_food = sum(e["food"] for e in by_type.get("grow", []))
    choices: dict[int, Game] = {}
    for etype in ELEMENT_TYPES:
        existing = len(by_type.get(etype, []))
        if existing <= grower_food:
            next_game = gs.set_action_field(game, "element", etype)
            choices[_elem_idx(etype)] = next_game
    return choices


def _grow_from_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    org_turns = game["state"]["player_turn"]["organism_turns"]
    last_action = org_turns[-1]["actions"][-1]["action"]
    etype = last_action.get("element")
    if not etype:
        return {}

    by_type = {t: [e for e in elements if e["type"] == t] for t in ELEMENT_TYPES}
    existing = len(by_type.get(etype, []))
    growers = by_type.get("grow", [])
    contributions = gs.food_contributions(growers, existing)

    choices: dict[int, Game] = {}
    for i, contrib in enumerate(contributions[:10]):
        next_game = gs.set_action_field(game, "from", contrib)
        choices[_GROW_FROM_BASE() + i] = next_game
    return choices


def _grow_to_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    growers = [e for e in elements if e["type"] == "grow"]
    growable = gs.growable_spaces(game, [e["space"] for e in growers])
    choices: dict[int, Game] = {}
    for space in growable:
        next_game = gs.set_action_field(game, "to", space)
        choices[_space_idx(game, space)] = next_game
    return choices


def _move_from_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    choices: dict[int, Game] = {}
    for el in elements:
        if gs.can_move(game, el["space"]):
            next_game = gs.set_action_field(game, "from", el["space"])
            choices[_space_idx(game, el["space"])] = next_game
    return choices


def _move_to_choices(game: Game) -> dict[int, Game]:
    org_turns = game["state"]["player_turn"]["organism_turns"]
    last_action = org_turns[-1]["actions"][-1]["action"]
    from_space = last_action.get("from")
    if not from_space:
        return {}
    choices: dict[int, Game] = {}
    for space in gs.available_spaces(game, from_space):
        next_game = gs.set_action_field(game, "to", space)
        choices[_space_idx(game, space)] = next_game
    return choices


def _circulate_from_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    fed = [e for e in elements if gs.fed_element(e)]
    choices: dict[int, Game] = {}
    for el in fed:
        next_game = gs.set_action_field(game, "from", el["space"])
        choices[_space_idx(game, el["space"])] = next_game
    return choices


def _circulate_to_choices(game: Game, elements: list[dict]) -> dict[int, Game]:
    org_turns = game["state"]["player_turn"]["organism_turns"]
    last_action = org_turns[-1]["actions"][-1]["action"]
    from_space = last_action.get("from")
    if not from_space:
        return {}
    choices: dict[int, Game] = {}
    for el in elements:
        if gs.open_element(el) and el["space"] != from_space:
            next_game = gs.set_action_field(game, "to", el["space"])
            choices[_space_idx(game, el["space"])] = next_game
    return choices


FIELD_CHOICE_FNS = {
    ("eat", "to"):         lambda game, els: _eat_to_choices(game, els),
    ("eat", "from"):       lambda game, els: _eat_from_choices(game),
    ("grow", "element"):   lambda game, els: _grow_element_choices(game, els),
    ("grow", "from"):      lambda game, els: _grow_from_choices(game, els),
    ("grow", "to"):        lambda game, els: _grow_to_choices(game, els),
    ("move", "from"):      lambda game, els: _move_from_choices(game, els),
    ("move", "to"):        lambda game, els: _move_to_choices(game),
    ("circulate", "from"): lambda game, els: _circulate_from_choices(game, els),
    ("circulate", "to"):   lambda game, els: _circulate_to_choices(game, els),
}


def _action_field_choices(game: Game, action_type: str, action_data: dict, elements: list[dict]) -> dict[int, Game]:
    field = _next_field(action_type, action_data)
    if not field:
        return {}
    fn = FIELD_CHOICE_FNS.get((action_type, field), lambda g, e: {})
    return fn(game, elements)


# ── main dispatch ────────────────────────────────────────────────────────────────

def find_state(game: Game) -> tuple[str, dict[int, Game]]:
    """Return (phase, {action_idx: next_game}).

    Terminal state returns ("game_over", {}).
    Automatic transitions return a single-entry dict.
    """
    state = game["state"]
    player_turn = state["player_turn"]
    player = player_turn["player"]
    advance = player_turn.get("advance")
    winner = state.get("winner")

    # ── terminal ──
    if winner:
        return ("game_over", {})

    # ── check victory ──
    w = gs.victory(game)
    if w:
        import copy
        game2 = copy.deepcopy(game)
        game2["state"]["winner"] = w
        return ("game_over", {})

    # ── advance states (automatic) ──
    if advance == "resolve_conflicts":
        next_game = gs.check_integrity(game, player)
        return ("resolve_conflicts", {-1: next_game})

    if advance == "check_integrity":
        next_game = gs.start_next_turn(game)
        return ("check_integrity", {-2: next_game})

    # ── introduction ──
    organisms = gs.player_organisms(game, player)
    organism_turns = player_turn["organism_turns"]

    if not organisms:
        choices = _introduce_choices(game, player)
        return ("introduce", choices)

    # ── choose organism / action type ──
    if not organism_turns:
        game = gs.find_organisms(game)
        organisms = gs.player_organisms(game, player)

        if len(organisms) > 1:
            choices = _choose_organism_choices(game, list(organisms.keys()))
            return ("choose_organism", choices)
        else:
            only_org = next(iter(organisms))
            game = gs.choose_organism_action(game, only_org)
            choices = _choose_action_type_choices(game)
            return ("choose_action_type", choices)

    # ── within organism turn ──
    org_turn = organism_turns[-1]
    organism = org_turn["organism"]
    choice = org_turn["choice"]
    num_actions = org_turn["num_actions"]
    actions = org_turn["actions"]

    organisms_full = gs.player_organisms(game, player)
    elements = organisms_full.get(organism, [])

    if choice is None:
        choices = _choose_action_type_choices(game)
        return ("choose_action_type", choices)

    if all(gs.complete_action(a) for a in actions):
        if len(actions) < num_actions:
            # Another action slot for this organism
            choices = _choose_action_choices(game, choice)
            if not choices:
                # Force pass
                pg = gs.choose_action_action(game, "circulate")
                pg = gs.pass_action(pg)
                choices = {_PASS_IDX(): pg}
            return ("choose_action", choices)

        elif len(organism_turns) < len(organisms_full):
            # More organisms to act
            acted = {t["organism"] for t in organism_turns}
            remaining = [o for o in organisms_full if o not in acted]
            choices = _choose_organism_choices(game, remaining)
            return ("choose_organism", choices)

        else:
            # All organisms done
            next_game = gs.resolve_conflicts(game, player)
            return ("actions_complete", {-3: next_game})

    else:
        # Fill in the next field of the last incomplete action
        last_action = actions[-1]
        atype = last_action["type"]
        adata = last_action["action"]

        if atype == "circulate" and adata.get("pass"):
            # Already passed, should not get here
            next_game = gs.resolve_conflicts(game, player)
            return ("actions_complete", {-3: next_game})

        choices = _action_field_choices(game, atype, adata, elements)
        if not choices:
            pg = gs.pass_action(game)
            choices = {_PASS_IDX(): pg}
        field = _next_field(atype, adata)
        return (f"{atype}_{field}", choices)
