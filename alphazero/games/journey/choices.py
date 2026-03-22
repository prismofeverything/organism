"""Legal moves for Journey — Python equivalent of journey/choice.cljc.

Each function returns {action_key -> next_state}.  Action keys are simple
Python values that will be serialised to integers by encoding.py.

This is a partial port covering the core turn loop.  Activate sub-phases
are stubbed and can be filled in progressively.
"""

from __future__ import annotations
from typing import Any

from . import hex as hx
from . import state as gs


def find_state(state: dict) -> tuple[str, dict]:
    """Return (phase, {action_key -> next_state}) for the current position.

    Mirrors Clojure choice/find-state.
    """
    phase = gs.current_phase(state)
    dispatch = {
        "choose_action_type":           _choose_action_type,
        "choose_move":                  _choose_move,
        "choose_launch_destination":    _choose_launch_destination,
        "choose_fly_from":              _choose_fly_from,
        "choose_fly_to":                _choose_fly_to,
        "choose_convert":               _choose_convert,
        "choose_activate":              _choose_activate,
        "choose_activate_self_bonus":   _choose_activate_self_bonus,
        "choose_activate_owner_bonus":  _choose_activate_owner_bonus,
        "draw_cards":                   _draw_cards,
        "choose_land":                  _choose_land,
        "game_over":                    lambda s: {},
    }
    fn = dispatch.get(phase, lambda s: {})
    return phase, fn(state)


# ── helpers ────────────────────────────────────────────────────────────────────

def _use_move_point(state: dict) -> dict:
    state = gs.deep_copy(state)
    pt = state["player_turn"].setdefault("action", {})
    pt["moves_remaining"] = pt.get("moves_remaining", 0) - 1
    state["player_turn"]["phase"] = "choose_move"
    return state


def _mobile_positions(state: dict, player: str) -> list[tuple]:
    board = state["board"]
    return [
        pos for pos, tile in board.items()
        if tile["sundivers"].get(player, 0) > 0
        and not gs.immobile(state, player, pos)
    ]


def _fly_destinations(state: dict, player: str, from_pos: tuple) -> set[tuple]:
    gates = gs.get_in(state, ["players", player, "gates"]) or {}
    gate_connected = gates.get(from_pos, set())
    adjacent = set(hx.neighbors(from_pos))
    return gate_connected | adjacent


# ── action-type ────────────────────────────────────────────────────────────────

def _choose_action_type(state: dict) -> dict:
    player = gs.current_player(state)
    choices = {}

    # Move
    mp = gs.move_points(state, player)
    move_s = gs.deep_copy(state)
    move_s["player_turn"]["phase"] = "choose_move"
    move_s["player_turn"].setdefault("action", {})["moves_remaining"] = mp
    choices["move"] = move_s

    # Convert (if patterns available)
    convs = gs.find_conversions(state, player)
    if convs:
        conv_s = gs.deep_copy(state)
        conv_s["player_turn"]["phase"] = "choose_convert"
        choices["convert"] = conv_s

    # Activate (if stations owned)
    stations = gs.get_in(state, ["players", player, "stations"]) or {}
    if stations:
        act_s = gs.deep_copy(state)
        act_s["player_turn"]["phase"] = "choose_activate"
        choices["activate"] = act_s

    return choices


# ── move ──────────────────────────────────────────────────────────────────────

def _choose_move(state: dict) -> dict:
    player = gs.current_player(state)
    remaining = gs.get_in(state, ["player_turn", "action", "moves_remaining"]) or 0
    habitat = gs.get_in(state, ["players", player, "habitat", "sundivers"]) or 0
    mobile = _mobile_positions(state, player)

    choices: dict = {}

    done_s = gs.deep_copy(state)
    done_s["player_turn"]["phase"] = "draw_cards"
    choices["done"] = done_s

    if remaining > 0 and habitat > 0:
        launch_s = gs.deep_copy(state)
        launch_s["player_turn"]["phase"] = "choose_launch_destination"
        choices["launch"] = launch_s

    if remaining > 0 and mobile:
        fly_s = gs.deep_copy(state)
        fly_s["player_turn"]["phase"] = "choose_fly_from"
        choices["fly"] = fly_s

    return choices


def _choose_launch_destination(state: dict) -> dict:
    player = gs.current_player(state)
    board = state["board"]
    ark = state["ark"]
    d = gs.heading_direction(state)
    flanks = [
        hx.add(ark, d),
        hx.add(ark, hx.rotate_ccw(d)),
        hx.add(ark, hx.rotate_cw(d)),
    ]
    positions = gs.launch_positions(state)
    wrapping_radius = state.get("wrapping_radius", hx.DEFAULT_WRAPPING_RADIUS)

    choices: dict = {}
    for pos in positions:
        after = gs.launch_sundiver(state, player, pos)
        after = _use_move_point(after)
        choices[pos] = after

        # Offer wrap for flank positions at the edge of known space
        if pos in flanks:
            fly_dir = d if pos == flanks[0] else (hx.rotate_ccw(d) if pos == flanks[1] else hx.rotate_cw(d))
            if not board.get(pos) and not any(
                hx.on_ray(ark, fly_dir, p) and hx.distance(ark, p) > 1
                for p in board
            ):
                wrap_pos = hx.wrap_target(set(board.keys()), wrapping_radius, ark, fly_dir)
                wrap_after = gs.launch_sundiver(state, player, wrap_pos)
                wrap_after = _use_move_point(wrap_after)
                choices[("wrap", pos)] = wrap_after

    return choices


def _choose_fly_from(state: dict) -> dict:
    player = gs.current_player(state)
    choices: dict = {}
    for pos in _mobile_positions(state, player):
        s = gs.deep_copy(state)
        s["player_turn"]["phase"] = "choose_fly_to"
        s["player_turn"]["action"]["fly_from"] = pos
        choices[pos] = s
    return choices


def _choose_fly_to(state: dict) -> dict:
    player = gs.current_player(state)
    from_pos = gs.get_in(state, ["player_turn", "action", "fly_from"])
    board = state["board"]
    wrapping_radius = state.get("wrapping_radius", hx.DEFAULT_WRAPPING_RADIUS)

    def after_fly(s: dict) -> dict:
        s = gs.deep_copy(s)
        s["player_turn"]["action"].pop("fly_from", None)
        return _use_move_point(s)

    choices: dict = {}
    for to_pos in _fly_destinations(state, player, from_pos):
        owner = gs.gate_owner(state, from_pos, to_pos)
        if owner:
            # fly through existing gate
            next_s = after_fly(gs.fly_through_gate(state, player, from_pos, to_pos))
        else:
            next_s = after_fly(gs.fly_sundiver(state, player, from_pos, to_pos))
        choices[to_pos] = next_s

        # Wrap offer for empty edge positions
        if not owner and not board.get(to_pos):
            dir_idx = hx.adjacent_direction(from_pos, to_pos)
            if dir_idx is not None:
                fly_dir = hx.HEX_DIRECTIONS[dir_idx]
                if not any(
                    hx.on_ray(from_pos, fly_dir, p) and hx.distance(from_pos, p) > 1
                    for p in board
                ):
                    wrap_pos = hx.wrap_target(set(board.keys()), wrapping_radius, from_pos, fly_dir)
                    wrap_next = after_fly(gs.fly_sundiver(state, player, from_pos, wrap_pos))
                    choices[("wrap", to_pos)] = wrap_next

    return choices



# ── convert ───────────────────────────────────────────────────────────────────

def _choose_convert(state: dict) -> dict:
    player = gs.current_player(state)
    choices: dict = {}
    for conv in gs.find_conversions(state, player):
        key = (conv["type"], tuple(conv["sundivers"]))
        choices[key] = gs.convert(state, player, conv["type"], conv["target"], conv["sundivers"])
    return choices


# ── activate (stub) ───────────────────────────────────────────────────────────

def _choose_activate(state: dict) -> dict:
    """Stub: skip activation and proceed to draw."""
    player = gs.current_player(state)
    stations = gs.get_in(state, ["players", player, "stations"]) or {}
    choices: dict = {}
    for pos, info in stations.items():
        s = gs.deep_copy(state)
        s["player_turn"]["phase"] = "draw_cards"  # simplified
        choices[("activate", pos)] = s
    return choices


def _choose_activate_self_bonus(state: dict) -> dict:
    bonus_total = gs.get_in(state, ["player_turn", "action", "bonus_total"]) or 0
    choices: dict = {}
    for n in range(bonus_total + 1):
        s = gs.deep_copy(state)
        s["player_turn"]["phase"] = "draw_cards"
        choices[n] = s
    return choices


def _choose_activate_owner_bonus(state: dict) -> dict:
    bonus_total = gs.get_in(state, ["player_turn", "action", "bonus_total"]) or 0
    choices: dict = {}
    for n in range(bonus_total + 1):
        s = gs.deep_copy(state)
        s["player_turn"]["phase"] = "draw_cards"
        choices[n] = s
    return choices


# ── draw cards / end turn ──────────────────────────────────────────────────────

def _draw_cards(state: dict) -> dict:
    """Draw one card and advance to the next player's turn."""
    s = gs.deep_copy(state)
    player = gs.current_player(s)
    if s["deck"]:
        card = s["deck"].pop(0)
        s["players"][player]["held_card"] = card
    landings = gs.available_landings(s)
    if landings:
        s["player_turn"]["phase"] = "choose_land"
        return {"draw": s}
    return {"draw": gs.advance_turn(s)}


def _choose_land(state: dict) -> dict:
    landings = gs.available_landings(state)
    choices: dict = {}
    for pos in landings:
        choices[("land", pos)] = gs.land_ark(state, pos)
    # Always offer continuing
    choices["continue"] = gs.advance_turn(state)
    return choices
