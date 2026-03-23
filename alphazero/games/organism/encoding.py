"""State and action encoding for Organism AlphaZero.

Tensor shape: (NUM_CHANNELS, NUM_RINGS, MAX_STEPS_PER_RING)
  = (NUM_CHANNELS, 7, 30)   for a 5-player 7-ring board

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
  N+14 .. N+23      grow-from contribution index (slots 0-9)
  N+24 .. N+33      choose-organism by ID mod 10

Total: N + 34
"""

from __future__ import annotations
import numpy as np

import alphazero.games.organism.choices as ch

NUM_RINGS = 7
MAX_STEPS = 30   # ring 6 has 6*5=30 spaces
# Pad to square so the shared AlphaZeroNetwork (which requires H==W) works.
GRID_SIZE = MAX_STEPS  # 30×30 square grid; ring dimension fits in first 7 rows

# Channels per player: has_element, is_eat, is_grow, is_move, element_food
CHANNELS_PER_PLAYER = 5
# Global channels: free_food, is_current_player_space, *capture_progress×num_players
GLOBAL_CHANNELS = 2  # free_food, current_player indicator
# Capture progress channels: one per player (broadcasted scalar)
CAPTURE_CHANNELS = 5  # for 5-player game


def build_space_index(all_spaces: list) -> tuple[dict, list]:
    """Build bidirectional mapping between spaces and integer indices."""
    space_to_idx = {space: idx for idx, space in enumerate(all_spaces)}
    idx_to_space = list(all_spaces)
    return space_to_idx, idx_to_space


def num_players_from_game(game: dict) -> int:
    return len(game["turn_order"])


def total_channels(num_players: int) -> int:
    return num_players * CHANNELS_PER_PLAYER + GLOBAL_CHANNELS + num_players


def action_space_size(num_spaces: int) -> int:
    return num_spaces + 34


def encode_state(game: dict, player: str) -> np.ndarray:
    """Encode game state as a (C, GRID_SIZE, GRID_SIZE) float32 tensor.

    Rings are placed in the first NUM_RINGS rows; remaining rows are zero-padded.
    This produces a square (30×30) grid compatible with AlphaZeroNetwork.
    """
    num_players = num_players_from_game(game)
    turn_order = game["turn_order"]
    C = total_channels(num_players)
    tensor = np.zeros((C, GRID_SIZE, GRID_SIZE), dtype=np.float32)

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
        if ring_idx >= NUM_RINGS or step >= MAX_STEPS:
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
        tensor[base + 4, ring_idx, step] = min(el["food"], 10) / 10.0  # normalized

    # Free food channel
    free_food_ch = num_players * CHANNELS_PER_PLAYER
    for space, food in food_map.items():
        ring_idx, step = space
        if ring_idx < NUM_RINGS and step < MAX_STEPS and space in game["adjacencies"]:
            tensor[free_food_ch, ring_idx, step] = min(food, 5) / 5.0

    # Current player indicator (all spaces where current player has an element)
    curr_player_ch = free_food_ch + 1
    for space, el in elements.items():
        ring_idx, step = space
        if el["player"] == player and ring_idx < NUM_RINGS and step < MAX_STEPS:
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
