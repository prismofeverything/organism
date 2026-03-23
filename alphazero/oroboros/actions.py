"""Programmatic action types: predicates + effects.

Each ActionType is a small declarative program that specifies:
  - WHO can act (requires own piece? target occupied by whom?)
  - WHERE (range, jump, direction constraints)
  - WHAT COST (resource consumption)
  - WHAT EFFECT (relocate, remove, spawn, convert, push, swap, transfer)

The game engine evaluates these generically — no hardcoded action logic.
"""

from __future__ import annotations
import math
from dataclasses import dataclass
from typing import Any


# ── Target filters ────────────────────────────────────────────────────────────────

TARGET_OWNERS = ["empty", "enemy", "self", "any"]

# ── Effects ───────────────────────────────────────────────────────────────────────
# Each effect is a string key. The game engine interprets it.

EFFECTS = [
    "relocate",         # move actor to target space
    "remove_target",    # destroy target piece, actor stays
    "spawn",            # create new piece at target (actor stays, pays cost)
    "transfer_resource",# move 1 resource from actor to target piece
    "collect",          # pick up free resource from target space
    "convert",          # change target piece to actor's player
    "push",             # push target piece one step away from actor
    "swap",             # swap positions with target piece
    "place",            # place a new piece (no actor required — for placement games)
]

# ── Direction constraints ─────────────────────────────────────────────────────────

DIRECTIONS = ["all", "forward", "lateral"]
# "all" = any adjacent; "forward" = away from board center; "lateral" = same ring


@dataclass(frozen=True)
class ActionType:
    """A declarative action rule.

    The game engine generates legal actions by:
      1. For each piece owned by current player (or each empty space if not requires_piece):
      2. For each reachable node within max_range satisfying direction constraint:
      3. Check target_owner filter
      4. Check resource cost is affordable
      5. If all pass, this is a legal action; apply effect.
    """
    name:             str
    requires_piece:   bool   = True   # actor must have own piece at source?
    target_owner:     str    = "empty" # "empty" | "enemy" | "self" | "any"
    max_range:        int    = 1       # how many steps to reach target (1 = adjacent only)
    can_jump:         bool   = False   # can skip over occupied cells?
    direction:        str    = "all"   # "all" | "forward" | "lateral"
    resource_cost:    int    = 0       # resource units consumed from actor piece
    resource_type:    str    = "food"  # which resource to consume
    effect:           str    = "relocate"
    spawn_type:       int    = -1      # for "spawn": type index of new piece (-1 = same as actor)
    # Type filter: if >= 0, only pieces of this type can perform this action
    actor_type_filter: int   = -1      # -1 = any type
    # Conflict check: for "remove_target", require attacker beats defender?
    requires_beats:   bool   = False

    def description_bits(self) -> float:
        """Information content of this action type."""
        bits = 0.0
        bits += 1.0  # requires_piece
        bits += math.log2(len(TARGET_OWNERS))
        bits += math.log2(4)  # max_range 1-4
        bits += 1.0  # can_jump
        bits += math.log2(len(DIRECTIONS))
        bits += math.log2(4)  # resource_cost 0-3
        bits += math.log2(len(EFFECTS))
        bits += 1.0  # requires_beats
        return bits

    def short_label(self) -> str:
        return self.name[0].upper()


# ── Predefined action types (matching the old hardcoded ones) ─────────────────────

MOVE = ActionType(name="move", target_owner="empty", effect="relocate")
EAT = ActionType(name="eat", target_owner="empty", effect="collect")  # collects from space
GROW = ActionType(name="grow", target_owner="empty", effect="spawn", resource_cost=1)
CAPTURE = ActionType(name="capture", target_owner="enemy", effect="remove_target", requires_beats=True)
CIRCULATE = ActionType(name="circulate", target_owner="self", effect="transfer_resource", resource_cost=1)
PLACE = ActionType(name="place", requires_piece=False, target_owner="empty", effect="place")

# ── Novel action types the search can discover ────────────────────────────────────

PUSH = ActionType(name="push", target_owner="enemy", effect="push")
SWAP = ActionType(name="swap", target_owner="any", effect="swap", max_range=1)
CONVERT = ActionType(name="convert", target_owner="enemy", effect="convert", resource_cost=2, requires_beats=True)
SNIPE = ActionType(name="snipe", target_owner="enemy", effect="remove_target", max_range=2, requires_beats=True)
LEAP = ActionType(name="leap", target_owner="empty", effect="relocate", max_range=2, can_jump=True)
CHARGE = ActionType(name="charge", target_owner="enemy", effect="remove_target", max_range=2, can_jump=False)

# ── Action catalog (the search samples from this) ────────────────────────────────

ACTION_CATALOG = [
    MOVE, EAT, GROW, CAPTURE, CIRCULATE, PLACE,
    PUSH, SWAP, CONVERT, SNIPE, LEAP, CHARGE,
]

ACTION_BY_NAME = {a.name: a for a in ACTION_CATALOG}
