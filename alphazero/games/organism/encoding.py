"""State and action encoding for Organism AlphaZero.

Tensor shape: (6 * players + 22, grid_size, grid_size).
The grid includes every ring position (36 square for the standard sixfold,
seven-ring board). Twenty context planes encode the active organism/action.

Action space layout:
  0 .. N-1          space selection (N = number of board spaces)
  N+0 .. N+5        introduce permutation 0-5
  N+6               action type "eat"
  N+7               action type "grow"
  N+8               action type "move"
  N+9               action type "circulate"
  N+10              grow element "eat"
  N+11              grow element "grow"
  N+12              grow element "move"
  N+13              pass
  N+14              zero-cost growth allocation
  N+15 .. N+23      reserved legacy slots
  Growth food donors use space indices, one food at a time.
  N+24 .. N+33      reserved legacy slots
  Organism selection uses its lowest occupied space index.

Total: N + 34
"""

from __future__ import annotations
import numpy as np

import alphazero.games.organism.choices as ch

# Channels per player: has_element, is_eat, is_grow, is_move, element_food
CHANNELS_PER_PLAYER = 5
# Global channels: free_food, is_current_player_space, *capture_progress×num_players
GLOBAL_CHANNELS = 2  # free_food, current_player indicator


def build_space_index(all_spaces: list) -> tuple[dict, list]:
    """Build bidirectional mapping between spaces and integer indices."""
    space_to_idx = {space: idx for idx, space in enumerate(all_spaces)}
    idx_to_space = list(all_spaces)
    return space_to_idx, idx_to_space


def num_players_from_game(game: dict) -> int:
    return len(game["turn_order"])


def total_channels(num_players: int) -> int:
    return num_players * CHANNELS_PER_PLAYER + GLOBAL_CHANNELS + num_players + 20


def action_space_size(num_spaces: int) -> int:
    return num_spaces + 34


def encode_state(game: dict, player: str) -> np.ndarray:
    """Encode board and decision context in a square float32 tensor."""
    num_players = num_players_from_game(game)
    turn_order = game["turn_order"]
    C = total_channels(num_players)
    grid_size = max(len(game["rings"]), max(s[1] for s in game["adjacencies"]) + 1)
    tensor = np.zeros((C, grid_size, grid_size), dtype=np.float32)

    # Reorder turn_order so current player is first
    if player in turn_order:
        p_idx = turn_order.index(player)
        ordered_players = turn_order[p_idx:] + turn_order[:p_idx]
    else:
        ordered_players = turn_order

    elements = game["state"]["elements"]
    food_map = game["state"]["food"]

    for space, el in elements.items():
        ring_idx, step = space
        if ring_idx >= grid_size or step >= grid_size:
            continue
        if space not in game["adjacencies"]:
            continue  # removed corner

        p = el["player"]
        if p not in ordered_players:
            continue
        pi = ordered_players.index(p)
        base = pi * CHANNELS_PER_PLAYER

        tensor[base + 0, ring_idx, step] = 1.0                          # has element
        tensor[base + 1, ring_idx, step] = 1.0 if el["type"] == "eat"  else 0.0
        tensor[base + 2, ring_idx, step] = 1.0 if el["type"] == "grow" else 0.0
        tensor[base + 3, ring_idx, step] = 1.0 if el["type"] == "move" else 0.0
        tensor[base + 4, ring_idx, step] = el["food"] / (10.0 + el["food"])  # retain differences above 10

    # Free food channel
    free_food_ch = num_players * CHANNELS_PER_PLAYER
    for space, food in food_map.items():
        ring_idx, step = space
        if ring_idx < grid_size and step < grid_size and space in game["adjacencies"]:
            tensor[free_food_ch, ring_idx, step] = food / (5.0 + food)

    # Current player indicator (all spaces where current player has an element)
    curr_player_ch = free_food_ch + 1
    for space, el in elements.items():
        ring_idx, step = space
        if el["player"] == player and ring_idx < grid_size and step < grid_size:
            if space in game["adjacencies"]:
                tensor[curr_player_ch, ring_idx, step] = 1.0

    # Capture progress (broadcasted as constant channels)
    capture_base = curr_player_ch + 1
    captures = game["state"]["captures"]
    for i, p in enumerate(ordered_players):
        if i >= num_players:
            break
        limit = game["players"][p].get("capture_limit", 5)
        progress = min(len(captures.get(p, [])) / max(limit, 1), 1.0)
        tensor[capture_base + i, :, :] = progress

    # Decision context: identical pieces can be selecting an action, its source,
    # or its destination. These planes distinguish those positions for the net.
    context = capture_base + num_players
    turn = game["state"]["player_turn"]
    turns = turn["organism_turns"]
    if turns:
        current = turns[-1]
        types = ["eat", "grow", "move", "circulate"]
        choice = current.get("choice")
        if choice in types:
            tensor[context + types.index(choice), :, :] = 1
        actions = current.get("actions", [])
        if actions:
            action = actions[-1]
            if action.get("type") in types:
                tensor[context + 4 + types.index(action["type"]), :, :] = 1
            fields = action.get("action", {})
            if fields.get("element") in types[:3]:
                tensor[context + 8 + types.index(fields["element"]), :, :] = 1
            for offset, key in [(11, "from"), (12, "to")]:
                value = fields.get(key)
                if isinstance(value, tuple) and value in game["adjacencies"]:
                    tensor[context + offset, value[0], value[1]] = 1
                elif isinstance(value, dict):
                    for space, amount in value.items():
                        if space in game["adjacencies"]:
                            tensor[context + offset, space[0], space[1]] = amount / 10
            tensor[context + 17, :, :] = float("from" in fields)
            tensor[context + 18, :, :] = float("to" in fields)
        acted = {t["organism"] for t in turns[:-1]}
        for space, element in elements.items():
            if element["player"] == turn["player"] and space in game["adjacencies"]:
                if element["organism"] == current["organism"]:
                    tensor[context + 13, space[0], space[1]] = 1
                if element["organism"] in acted:
                    tensor[context + 14, space[0], space[1]] = 1
        tensor[context + 15, :, :] = len(actions) / 10
        tensor[context + 16, :, :] = current.get("num_actions", 0) / 10
    for space, amount in game["state"].get("az_grow_from", {}).items():
        tensor[context + 11, space[0], space[1]] = amount / 10
    tensor[context + 19, :, :] = float(bool(turns))
    return tensor


def action_to_index(game: dict, action_key: int) -> int:
    """Identity — action keys from choices.py are already indices."""
    return action_key


def index_to_action(game: dict, idx: int) -> int:
    """Identity — indices map directly to action keys."""
    return idx


def patch_choices_n(game: dict) -> None:
    """Set choices._N to the number of board spaces for this game."""
    ch._N = len(game["space_to_idx"])
