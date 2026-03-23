"""Legal moves for Journey — Python port of journey/choice.cljc.

Each function returns {action_key -> next_state}.
find_state is the central dispatch mirroring Clojure choice/find-state.
"""

from __future__ import annotations
from typing import Any

from . import hex as hx
from . import state as gs


def find_state(state: dict) -> tuple[str, dict]:
    """Return (phase, {action_key -> next_state}) for the current position."""
    phase = gs.current_phase(state)
    dispatch = {
        "choose_action_type":               _choose_action_type,
        "choose_move":                      _choose_move,
        "choose_launch_destination":        _choose_launch_destination,
        "choose_fly_from":                  _choose_fly_from,
        "choose_fly_to":                    _choose_fly_to,
        "choose_convert":                   _choose_convert,
        "choose_activate":                  _choose_activate,
        "choose_activate_self_bonus":       _choose_activate_self_bonus,
        "choose_activate_owner_bonus":      _choose_activate_owner_bonus,
        "choose_activate_matrix_beacon":    _choose_activate_matrix_beacon,
        "choose_activate_matrix_spend":     _choose_activate_matrix_spend,
        "choose_activate_tower_heading":    _choose_activate_tower_heading,
        "choose_activate_tower_join":       _choose_activate_tower_join,
        "choose_activate_tower_join_spend": _choose_activate_tower_join_spend,
        "choose_activate_tower_spend":      _choose_activate_tower_spend,
        "draw_cards":                       _draw_cards,
        "flare_beacon_join":                _flare_beacon_join,
        "flare_beacon_join_spend":          _flare_beacon_join_spend,
        "keep_card":                        _keep_card,
        "choose_captain_drift":             _choose_captain_drift,
        "choose_ark_advance":               _choose_ark_advance,
        "choose_flare_advance":             _choose_flare_advance,
        "draw_drift_card":                  _draw_drift_card,
        "choose_drift_flare_advance":       _choose_drift_flare_advance,
        "captain_beacon_join":              _captain_beacon_join,
        "captain_beacon_join_spend":        _captain_beacon_join_spend,
        "cipher":                           _cipher,
        "cipher_spend":                     _cipher_spend,
        "choose_land":                      _choose_land,
        "game_over":                        lambda s: {},
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
    return [
        pos for pos, tile in state["board"].items()
        if tile["sundivers"].get(player, 0) > 0
        and not gs.immobile(state, player, pos)
    ]


def _fly_destinations(state: dict, player: str, from_pos: tuple) -> set[tuple]:
    gates = gs.get_in(state, ["players", player, "gates"]) or {}
    return gates.get(from_pos, set()) | set(hx.neighbors(from_pos))


def _current_actor(state: dict) -> str:
    return gs.get_in(state, ["player_turn", "action", "current_actor"])


def _actor_player(state: dict) -> str:
    return gs.actor_player(state, _current_actor(state))


def _actor_actions_key(state: dict) -> str:
    return "activator_actions" if _current_actor(state) == "activator" else "owner_actions"


# ── action type ────────────────────────────────────────────────────────────────

def _choose_action_type(state: dict) -> dict:
    """Only offer action types that have at least one legal sub-choice."""
    choices: dict = {}
    for action_type in gs.ACTION_TYPES:
        next_state = gs.choose_action_type(state, action_type)
        sub = {
            "move":     _choose_move,
            "convert":  _choose_convert,
            "activate": _choose_activate,
        }[action_type](next_state)
        if sub:
            choices[action_type] = next_state
    return choices


# ── move ──────────────────────────────────────────────────────────────────────

def _choose_move(state: dict) -> dict:
    player = gs.current_player(state)
    remaining = gs.get_in(state, ["player_turn", "action", "moves_remaining"]) or 0
    habitat = gs.get_in(state, ["players", player, "habitat", "sundivers"]) or 0
    mobile = _mobile_positions(state, player)

    done_s = gs.deep_copy(state)
    done_s["player_turn"]["phase"] = "draw_cards"
    choices: dict = {"done": done_s}

    if remaining > 0 and habitat > 0:
        s = gs.deep_copy(state)
        s["player_turn"]["phase"] = "choose_launch_destination"
        choices["launch"] = s

    if remaining > 0 and mobile:
        s = gs.deep_copy(state)
        s["player_turn"]["phase"] = "choose_fly_from"
        choices["fly"] = s

    return choices


def _choose_launch_destination(state: dict) -> dict:
    player = gs.current_player(state)
    board = state["board"]
    ark = state["ark"]
    d = gs.heading_direction(state)
    flanks = [hx.add(ark, d), hx.add(ark, hx.rotate_ccw(d)), hx.add(ark, hx.rotate_cw(d))]
    flank_dirs = [d, hx.rotate_ccw(d), hx.rotate_cw(d)]
    positions = gs.launch_positions(state)

    choices: dict = {}
    for pos in positions:
        choices[pos] = _use_move_point(gs.launch_sundiver(state, player, pos))

        if pos in flanks:
            fly_dir = flank_dirs[flanks.index(pos)]
            if not board.get(pos) and not any(
                hx.on_ray(ark, fly_dir, p) and hx.distance(ark, p) > 1 for p in board
            ):
                wrap_pos = gs.wrap_target(state, ark, fly_dir)
                choices[("wrap", pos)] = _use_move_point(gs.launch_sundiver(state, player, wrap_pos))
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

    def after_fly(s: dict) -> dict:
        s = gs.deep_copy(s)
        s["player_turn"]["action"].pop("fly_from", None)
        return _use_move_point(s)

    choices: dict = {}
    for to_pos in _fly_destinations(state, player, from_pos):
        owner = gs.gate_owner(state, from_pos, to_pos)
        if owner:
            next_s = after_fly(gs.fly_through_gate(state, player, from_pos, to_pos))
        else:
            next_s = after_fly(gs.fly_sundiver(state, player, from_pos, to_pos))
        choices[to_pos] = next_s

        if not owner and not board.get(to_pos):
            dir_idx = hx.adjacent_direction(from_pos, to_pos)
            if dir_idx is not None:
                fly_dir = hx.HEX_DIRECTIONS[dir_idx]
                if not any(
                    hx.on_ray(from_pos, fly_dir, p) and hx.distance(from_pos, p) > 1
                    for p in board
                ):
                    wrap_pos = gs.wrap_target(state, from_pos, fly_dir)
                    choices[("wrap", to_pos)] = after_fly(
                        gs.fly_sundiver(state, player, from_pos, wrap_pos))
    return choices


# ── convert ───────────────────────────────────────────────────────────────────

def _choose_convert(state: dict) -> dict:
    player = gs.current_player(state)
    choices: dict = {}
    for conv in gs.find_conversions(state, player):
        key = (conv["type"], tuple(conv["sundivers"]))
        choices[key] = gs.convert(state, player, conv["type"], conv["target"], conv["sundivers"])
    return choices


# ── activate ──────────────────────────────────────────────────────────────────

def _choose_activate(state: dict) -> dict:
    player = gs.current_player(state)
    choices: dict = {}
    for stype in gs.activatable_station_types(state, player):
        choices[stype] = gs.start_activate(state, stype)
    return choices


def _choose_activate_self_bonus(state: dict) -> dict:
    bonus_total = gs.get_in(state, ["player_turn", "action", "bonus_total"]) or 0
    base = gs.get_in(state, ["player_turn", "action", "activator_actions"]) or 0
    choices: dict = {}
    for n in range(bonus_total + 1):
        s = gs.deep_copy(state)
        s["player_turn"]["action"]["activator_actions"] = base + n
        choices[n] = gs.begin_actor_actions(s, "activator")
    return choices


def _choose_activate_owner_bonus(state: dict) -> dict:
    bonus_total = gs.get_in(state, ["player_turn", "action", "bonus_total"]) or 0
    owner = gs.get_in(state, ["player_turn", "action", "current_owner"])
    station_type = gs.get_in(state, ["player_turn", "action", "station_type"])

    if station_type == "matrix":
        positions = len(gs.matrix_beacon_positions(state, owner))
        spendable = gs.total_spendable_sundivers(state, owner)
        beacons = gs.get_in(state, ["players", owner, "reserve", "beacons"]) or 0
        max_feasible = min(bonus_total, positions, spendable, beacons)
    elif station_type == "tower":
        max_feasible = min(bonus_total, gs.total_spendable_sundivers(state, owner))
    else:
        max_feasible = bonus_total

    choices: dict = {}
    for n in range(max_feasible + 1):
        s = gs.deep_copy(state)
        s["player_turn"]["action"]["owner_actions"] = n
        s["player_turn"]["choice_player"] = None
        choices[n] = gs.begin_actor_actions(s, "owner")
    return choices


# ── matrix activation ─────────────────────────────────────────────────────────

def _decrement_matrix_and_continue(state: dict) -> dict:
    actor = _current_actor(state)
    act_key = _actor_actions_key(state)
    s = gs.deep_copy(state)
    s["player_turn"]["action"][act_key] -= 1
    if s["player_turn"]["action"].get(act_key, 0) > 0:
        s["player_turn"]["phase"] = "choose_activate_matrix_beacon"
        return s
    return gs.advance_after_actions(s, actor)


def _choose_activate_matrix_beacon(state: dict) -> dict:
    player = _actor_player(state)
    choices: dict = {}
    for pos in gs.matrix_beacon_positions(state, player):
        s = gs.place_beacon(state, player, pos)
        s = gs.deep_copy(s)
        s["player_turn"]["phase"] = "choose_activate_matrix_spend"
        choices[pos] = s
    return choices


def _choose_activate_matrix_spend(state: dict) -> dict:
    player = _actor_player(state)
    choices: dict = {}
    for pos in gs.sundiver_spend_positions(state, player):
        choices[pos] = _decrement_matrix_and_continue(gs.spend_sundiver(state, player, pos))
    return choices


# ── tower activation ──────────────────────────────────────────────────────────

def _tower_continue(state: dict) -> dict:
    actor = _current_actor(state)
    remaining = gs.get_in(state, ["player_turn", "action", _actor_actions_key(state)]) or 0
    if remaining > 0:
        state = gs.deep_copy(state)
        state["player_turn"]["phase"] = "choose_activate_tower_heading"
        return state
    landings = gs.available_landings(state)
    if landings:
        state = gs.deep_copy(state)
        state["player_turn"]["action"]["post_land_actor"] = actor
        state["player_turn"]["phase"] = "choose_land"
        return state
    return gs.advance_after_actions(state, actor)


def _tower_after_advance(state: dict) -> dict:
    act_player = _actor_player(state)
    new_ark = state["ark"]
    tile = gs.get_tile(state, new_ark)
    if tile and tile.get("beacon"):
        beacon_owner = tile["beacon"]
        s = gs.discover_beacon(state, new_ark, act_player)
        if beacon_owner == act_player:
            s = gs.deep_copy(s)
            s["player_turn"]["phase"] = "choose_activate_tower_spend"
            return s
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["action"]["pending_join_actor"] = _current_actor(s)
            s["player_turn"]["phase"] = "choose_activate_tower_join"
            return s
    state = gs.deep_copy(state)
    state["player_turn"]["phase"] = "choose_activate_tower_spend"
    return state


def _choose_activate_tower_heading(state: dict) -> dict:
    choices: dict = {}
    for turn_dir in ["none", "left", "right"]:
        s = gs.turn_heading(state, turn_dir)
        if gs.get_tile(s, s["heading_token"]):
            choices[turn_dir] = _tower_after_advance(gs.advance_ark(s))
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["ark_advance_context"] = "tower"
            s["player_turn"]["phase"] = "choose_ark_advance"
            choices[turn_dir] = s
    return choices


def _finish_tower_join(state: dict) -> dict:
    actor = gs.get_in(state, ["player_turn", "action", "pending_join_actor"])
    act_player = gs.actor_player(state, actor)
    s = gs.join_beacon_to_cipher(state, act_player)
    s = gs.deep_copy(s)
    s["player_turn"]["action"]["beacons_joined"] = \
        s["player_turn"]["action"].get("beacons_joined", 0) + 1
    for key in ["pending_join_actor", "join_spend_remaining"]:
        s["player_turn"]["action"].pop(key, None)
    s["player_turn"]["phase"] = "choose_activate_tower_spend"
    return s


def _choose_activate_tower_join(state: dict) -> dict:
    actor = gs.get_in(state, ["player_turn", "action", "pending_join_actor"])
    act_player = gs.actor_player(state, actor)
    join_cost = gs.get_in(state, ["player_turn", "action", "beacons_joined"]) or 0
    has_beacons = (gs.get_in(state, ["players", act_player, "reserve", "beacons"]) or 0) > 0
    can_afford = gs.total_spendable_sundivers(state, act_player) >= join_cost

    after_skip = gs.deep_copy(state)
    after_skip["player_turn"]["action"].pop("pending_join_actor", None)
    after_skip["player_turn"]["phase"] = "choose_activate_tower_spend"

    choices: dict = {"skip": after_skip}
    if has_beacons and can_afford:
        if join_cost == 0:
            choices["join"] = _finish_tower_join(state)
        else:
            s = gs.deep_copy(state)
            s["player_turn"]["action"]["join_spend_remaining"] = join_cost
            s["player_turn"]["phase"] = "choose_activate_tower_join_spend"
            choices["join"] = s
    return choices


def _choose_activate_tower_join_spend(state: dict) -> dict:
    actor = gs.get_in(state, ["player_turn", "action", "pending_join_actor"])
    act_player = gs.actor_player(state, actor)
    remaining = gs.get_in(state, ["player_turn", "action", "join_spend_remaining"]) or 0
    choices: dict = {}
    for pos in gs.sundiver_spend_positions(state, act_player):
        s = gs.spend_sundiver(state, act_player, pos)
        new_remaining = remaining - 1
        if new_remaining == 0:
            choices[pos] = _finish_tower_join(s)
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["action"]["join_spend_remaining"] = new_remaining
            s["player_turn"]["phase"] = "choose_activate_tower_join_spend"
            choices[pos] = s
    return choices


def _choose_activate_tower_spend(state: dict) -> dict:
    player = _actor_player(state)
    act_key = _actor_actions_key(state)
    choices: dict = {}
    for pos in gs.sundiver_spend_positions(state, player):
        s = gs.spend_sundiver(state, player, pos)
        s = gs.deep_copy(s)
        s["player_turn"]["action"][act_key] = \
            s["player_turn"]["action"].get(act_key, 0) - 1
        choices[pos] = _tower_continue(s)
    return choices


# ── post-action: draw cards ────────────────────────────────────────────────────

def _draw_cards(state: dict) -> dict:
    return {"draw": gs.process_draw_and_flares(state)}


# ── post-action: flare beacon joins ───────────────────────────────────────────

def _advance_flare_join_or_keep(state: dict) -> dict:
    remaining = gs.get_in(state, ["player_turn", "action", "flare_join_indices"]) or []
    if remaining:
        return state
    state = gs.deep_copy(state)
    state["player_turn"]["choice_player"] = None
    state["player_turn"]["phase"] = "keep_card"
    return state


def _finish_flare_join(state: dict, cipher_idx: int) -> dict:
    captain = state["captain_flame"]
    s = gs.join_beacon_to_cipher(state, captain, cipher_idx)
    s = gs.deep_copy(s)
    s["player_turn"]["captain_beacons_joined"] = \
        s["player_turn"].get("captain_beacons_joined", 0) + 1
    indices = list(s["player_turn"]["action"].get("flare_join_indices", []))
    if indices:
        indices.pop(0)
    s["player_turn"]["action"]["flare_join_indices"] = indices
    for key in ["flare_join_spend_remaining", "flare_join_cipher_idx"]:
        s["player_turn"]["action"].pop(key, None)
    return _advance_flare_join_or_keep(s)


def _flare_beacon_join(state: dict) -> dict:
    captain = state["captain_flame"]
    join_indices = gs.get_in(state, ["player_turn", "action", "flare_join_indices"]) or []
    if not join_indices:
        return {"skip": _advance_flare_join_or_keep(state)}
    cipher_idx = join_indices[0]
    join_cost = gs.get_in(state, ["player_turn", "captain_beacons_joined"]) or 0
    has_beacons = (gs.get_in(state, ["players", captain, "reserve", "beacons"]) or 0) > 0
    can_afford = gs.total_spendable_sundivers(state, captain) >= join_cost

    after_skip = gs.deep_copy(state)
    indices = list(after_skip["player_turn"]["action"].get("flare_join_indices", []))
    if indices:
        indices.pop(0)
    after_skip["player_turn"]["action"]["flare_join_indices"] = indices
    after_skip = _advance_flare_join_or_keep(after_skip)

    choices: dict = {"skip": after_skip}
    if has_beacons and can_afford:
        if join_cost == 0:
            choices["join"] = _finish_flare_join(state, cipher_idx)
        else:
            s = gs.deep_copy(state)
            s["player_turn"]["action"]["flare_join_cipher_idx"] = cipher_idx
            s["player_turn"]["action"]["flare_join_spend_remaining"] = join_cost
            s["player_turn"]["phase"] = "flare_beacon_join_spend"
            choices["join"] = s
    return choices


def _flare_beacon_join_spend(state: dict) -> dict:
    captain = state["captain_flame"]
    cipher_idx = gs.get_in(state, ["player_turn", "action", "flare_join_cipher_idx"])
    remaining = gs.get_in(state, ["player_turn", "action", "flare_join_spend_remaining"]) or 0
    choices: dict = {}
    for pos in gs.sundiver_spend_positions(state, captain):
        s = gs.spend_sundiver(state, captain, pos)
        new_rem = remaining - 1
        if new_rem == 0:
            choices[pos] = _finish_flare_join(s, cipher_idx)
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["action"]["flare_join_spend_remaining"] = new_rem
            s["player_turn"]["phase"] = "flare_beacon_join_spend"
            choices[pos] = s
    return choices


# ── post-action: keep card ─────────────────────────────────────────────────────

def _advance_to_captain_or_cipher(state: dict) -> dict:
    player = gs.current_player(state)
    captain = state["captain_flame"]
    if player == captain:
        state = gs.deep_copy(state)
        state["player_turn"]["phase"] = "choose_captain_drift"
        return state
    return _enter_cipher_phase(state)


def _keep_card(state: dict) -> dict:
    player = gs.current_player(state)
    drawn = gs.get_in(state, ["player_turn", "action", "drawn_cards"]) or []
    held = gs.get_in(state, ["players", player, "held_card"])

    if not drawn:
        return {"continue": _advance_to_captain_or_cipher(state)}

    choices: dict = {}
    for card in drawn:
        s = gs.deep_copy(state)
        s["players"][player]["held_card"] = card
        others = [c for c in drawn if c != card]
        s["discard"].extend(others)
        if held:
            s["discard"].append(held)
        choices[tuple(card.items())] = _advance_to_captain_or_cipher(s)

    if held:
        s = gs.deep_copy(state)
        s["discard"].extend(drawn)
        choices["keep_held"] = _advance_to_captain_or_cipher(s)

    return choices


# ── cipher phase ──────────────────────────────────────────────────────────────

def _pending_cipher_queue(pending_cipher: list) -> list:
    queue = []
    for entry in pending_cipher:
        queue.append({"player": entry["beacon_owner"], "color": entry["world"], "from_bag": False})
        for p in entry.get("joiners", []):
            queue.append({"player": p, "color": entry["world"], "from_bag": True})
    return queue


def _enter_cipher_phase(state: dict) -> dict:
    queue = _pending_cipher_queue(state.get("pending_cipher", []))
    if not queue:
        return gs.begin_next_player_turn(state)
    state = gs.deep_copy(state)
    state["player_turn"]["cipher_queue"] = queue
    state["player_turn"]["choice_player"] = queue[0]["player"]
    state["player_turn"]["phase"] = "cipher"
    return state


def _advance_cipher(state: dict) -> dict:
    queue = list(gs.get_in(state, ["player_turn", "cipher_queue"]) or [])
    if queue:
        queue.pop(0)
    if not queue:
        return gs.begin_next_player_turn(state)
    state = gs.deep_copy(state)
    state["player_turn"]["cipher_queue"] = queue
    state["player_turn"]["choice_player"] = queue[0]["player"]
    state["player_turn"]["phase"] = "cipher"
    return state


def _complete_cipher_placement(state: dict) -> dict:
    player = gs.get_in(state, ["player_turn", "cipher_pending_player"])
    pos = gs.get_in(state, ["player_turn", "cipher_pending_pos"])
    color = gs.get_in(state, ["player_turn", "cipher_pending_color"])
    from_bag = gs.get_in(state, ["player_turn", "cipher_pending_from_bag"])
    s = gs.cipher_place_beacon(state, player, pos, color, from_bag)
    s = gs.deep_copy(s)
    for key in ["cipher_pending_player", "cipher_pending_pos",
                "cipher_pending_color", "cipher_pending_from_bag", "cipher_spend_remaining"]:
        s["player_turn"].pop(key, None)
    return _advance_cipher(s)


def _cipher(state: dict) -> dict:
    queue = gs.get_in(state, ["player_turn", "cipher_queue"]) or []
    if not queue:
        return {"done": gs.begin_next_player_turn(state)}
    entry = queue[0]
    player, color, from_bag = entry["player"], entry["color"], entry["from_bag"]
    spendable = gs.total_spendable_sundivers(state, player)
    choices: dict = {}
    for pos in state["cipher"]:
        cost = gs.cipher_placement_cost(state, pos, color)
        if cost <= spendable:
            if cost == 0:
                choices[pos] = _advance_cipher(gs.cipher_place_beacon(state, player, pos, color, from_bag))
            else:
                s = gs.deep_copy(state)
                s["player_turn"].update({
                    "cipher_pending_player": player,
                    "cipher_pending_pos": pos,
                    "cipher_pending_color": color,
                    "cipher_pending_from_bag": from_bag,
                    "cipher_spend_remaining": cost,
                    "phase": "cipher_spend",
                })
                choices[pos] = s
    return choices if choices else {"skip": _advance_cipher(state)}


def _cipher_spend(state: dict) -> dict:
    player = gs.get_in(state, ["player_turn", "cipher_pending_player"])
    remaining = gs.get_in(state, ["player_turn", "cipher_spend_remaining"]) or 0
    choices: dict = {}
    for pos in gs.sundiver_spend_positions(state, player):
        s = gs.spend_sundiver(state, player, pos)
        new_rem = remaining - 1
        if new_rem == 0:
            choices[pos] = _complete_cipher_placement(s)
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["cipher_spend_remaining"] = new_rem
            s["player_turn"]["phase"] = "cipher_spend"
            choices[pos] = s
    return choices


# ── captain drift ─────────────────────────────────────────────────────────────

def _captain_drift_continuation(state: dict) -> dict:
    if gs.get_in(state, ["player_turn", "beacon_join_continuation"]) == "draw_drift_card":
        state = gs.deep_copy(state)
        state["player_turn"].pop("beacon_join_continuation", None)
        state["player_turn"]["phase"] = "draw_drift_card"
        return state
    return _enter_cipher_phase(state)


def _handle_captain_drift_beacon(state: dict) -> dict:
    captain = state["captain_flame"]
    new_ark = state["ark"]
    tile = gs.get_tile(state, new_ark)
    if tile and tile.get("beacon"):
        beacon_owner = tile["beacon"]
        s = gs.discover_beacon(state, new_ark, captain)
        if beacon_owner == captain:
            s = gs.deep_copy(s)
            s["player_turn"]["phase"] = "draw_drift_card"
            return s
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["beacon_join_continuation"] = "draw_drift_card"
            s["player_turn"]["phase"] = "captain_beacon_join"
            return s
    state = gs.deep_copy(state)
    state["player_turn"]["phase"] = "draw_drift_card"
    return state


def _choose_captain_drift(state: dict) -> dict:
    choices: dict = {}
    for turn_dir in ["none", "left", "right"]:
        s = gs.turn_heading(state, turn_dir)
        if gs.get_tile(s, s["heading_token"]):
            choices[turn_dir] = _handle_captain_drift_beacon(gs.advance_ark(s))
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["ark_advance_context"] = "drift"
            s["player_turn"]["phase"] = "choose_ark_advance"
            choices[turn_dir] = s
    return choices


def _finish_captain_join(state: dict) -> dict:
    captain = state["captain_flame"]
    s = gs.join_beacon_to_cipher(state, captain)
    s = gs.deep_copy(s)
    s["player_turn"]["captain_beacons_joined"] = \
        s["player_turn"].get("captain_beacons_joined", 0) + 1
    s["player_turn"]["action"].pop("captain_join_spend_remaining", None)
    return _captain_drift_continuation(s)


def _captain_beacon_join(state: dict) -> dict:
    captain = state["captain_flame"]
    join_cost = gs.get_in(state, ["player_turn", "captain_beacons_joined"]) or 0
    has_beacons = (gs.get_in(state, ["players", captain, "reserve", "beacons"]) or 0) > 0
    can_afford = gs.total_spendable_sundivers(state, captain) >= join_cost
    choices: dict = {"skip": _captain_drift_continuation(state)}
    if has_beacons and can_afford:
        if join_cost == 0:
            choices["join"] = _finish_captain_join(state)
        else:
            s = gs.deep_copy(state)
            s["player_turn"]["action"]["captain_join_spend_remaining"] = join_cost
            s["player_turn"]["phase"] = "captain_beacon_join_spend"
            choices["join"] = s
    return choices


def _captain_beacon_join_spend(state: dict) -> dict:
    captain = state["captain_flame"]
    remaining = gs.get_in(state, ["player_turn", "action", "captain_join_spend_remaining"]) or 0
    choices: dict = {}
    for pos in gs.sundiver_spend_positions(state, captain):
        s = gs.spend_sundiver(state, captain, pos)
        new_rem = remaining - 1
        if new_rem == 0:
            choices[pos] = _finish_captain_join(s)
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["action"]["captain_join_spend_remaining"] = new_rem
            s["player_turn"]["phase"] = "captain_beacon_join_spend"
            choices[pos] = s
    return choices


# ── drift card ────────────────────────────────────────────────────────────────

def _process_drift_flare_advance(state: dict, new_ark: tuple) -> dict:
    captain = state["captain_flame"]
    tile = gs.get_tile(state, new_ark)
    s = gs.advance_ark_to(state, new_ark)
    if tile and tile.get("beacon"):
        beacon_owner = tile["beacon"]
        s = gs.discover_beacon(s, new_ark, captain)
        if beacon_owner == captain:
            return _enter_cipher_phase(s)
        else:
            s = gs.deep_copy(s)
            s["player_turn"]["phase"] = "captain_beacon_join"
            return s
    return _enter_cipher_phase(s)


def _draw_drift_card(state: dict) -> dict:
    s, cards = gs.draw_n_cards(state, 1)
    card = cards[0] if cards else None
    if card and gs.flare(card):
        s = gs.deep_copy(s)
        s["flares_drawn"] = (s.get("flares_drawn") or 0) + 1
        if s["flares_drawn"] >= 13:
            return {"draw": gs.trigger_loss(s)}
        next_pos = s["heading_token"]
        if gs.get_tile(s, next_pos):
            return {"draw": _process_drift_flare_advance(s, next_pos)}
        else:
            s["player_turn"]["phase"] = "choose_drift_flare_advance"
            return {"draw": s}
    return {"draw": _enter_cipher_phase(s)}


def _choose_drift_flare_advance(state: dict) -> dict:
    from_pos = state["ark"]
    d = gs.heading_direction(state)
    wrap_pos = gs.wrap_target(state, from_pos, d)
    next_pos = state["heading_token"]
    return {
        "direct": _process_drift_flare_advance(state, next_pos),
        "wrap":   _process_drift_flare_advance(state, wrap_pos),
    }


# ── ark advance wrap choice ────────────────────────────────────────────────────

def _choose_ark_advance(state: dict) -> dict:
    from_pos = state["ark"]
    d = gs.heading_direction(state)
    wrap_pos = gs.wrap_target(state, from_pos, d)
    context = gs.get_in(state, ["player_turn", "ark_advance_context"])
    after_fn = _tower_after_advance if context == "tower" else _handle_captain_drift_beacon
    return {
        "direct": after_fn(gs.advance_ark(state)),
        "wrap":   after_fn(gs.advance_ark_to(state, wrap_pos)),
    }


def _choose_flare_advance(state: dict) -> dict:
    from_pos = state["ark"]
    d = gs.heading_direction(state)
    wrap_pos = gs.wrap_target(state, from_pos, d)
    next_pos = state["heading_token"]
    return {
        "direct": gs.advance_flare_ark_to(state, next_pos),
        "wrap":   gs.advance_flare_ark_to(state, wrap_pos),
    }


# ── landing ───────────────────────────────────────────────────────────────────

def _choose_land(state: dict) -> dict:
    actor = gs.get_in(state, ["player_turn", "action", "post_land_actor"])
    landings = gs.available_landings(state)

    continue_s = gs.deep_copy(state)
    continue_s["player_turn"]["action"].pop("post_land_actor", None)
    continue_s = gs.advance_after_actions(continue_s, actor)

    choices: dict = {"continue": continue_s}
    for pos in landings:
        choices[("land", pos)] = gs.land_ark(state, pos)
    return choices
