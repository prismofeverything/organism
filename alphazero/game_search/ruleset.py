"""RuleSet: compact encoding of a parameterized abstract board game.

Description length K(R) is the information content of the parameter vector —
the denominator in the complexity ratio K(game-tree) / K(rules) that we
are trying to maximise.
"""

from __future__ import annotations
import math
from dataclasses import dataclass, field
from typing import Any

# ── parameter domains (used by Grammar and for description-length calculation)

BOARD_SYMMETRIES  = [3, 4, 5, 6]
NUM_RINGS         = [3, 4, 5, 6]
NUM_TYPES         = [2, 3, 4]
ELEMENTS_PER_PLAYER = [2, 3, 4, 5]
FOOD_INITIAL      = [0, 1, 2]
WIN_TYPES         = ["captures", "population"]
WIN_THRESHOLDS    = [3, 5, 7, 10]
NUM_PLAYERS       = [2, 3, 4, 5]
# Conflict outcomes per pair: 0=coexist 1=i_wins 2=j_wins 3=mutual_destruction
CONFLICT_VALUES   = [0, 1, 2, 3]


@dataclass(frozen=True)
class RuleSet:
    """Immutable description of a game's rules.

    conflict is a tuple of length num_types*(num_types-1)//2, one entry per
    unordered pair (i,j) with i<j, ordered lexicographically:
      (0,1), (0,2), ..., (0,T-1), (1,2), ..., (T-2,T-1)
    Values: 0=coexist  1=i beats j  2=j beats i  3=mutual destruction
    """
    # Board
    board_symmetry:     int   = 5
    num_rings:          int   = 5

    # Elements
    num_types:          int   = 3
    elements_per_player: int  = 3

    # Food
    food_enabled:       bool  = True
    food_initial:       int   = 1

    # Actions (which are available to players)
    can_move:           bool  = True
    can_eat:            bool  = True
    can_grow:           bool  = True
    can_capture:        bool  = False   # direct capture independent of adjacency conflict
    can_circulate:      bool  = True

    # Conflict resolution (element-adjacency triggered)
    # Default = Organism-style 3-type heterarchy: eat>grow, move>eat, grow>move
    #   pair(0,1)=1: type0(eat) beats type1(grow)
    #   pair(0,2)=2: type2(move) beats type0(eat)
    #   pair(1,2)=1: type1(grow) beats type2(move)
    conflict: tuple = (1, 2, 1)

    # Win condition
    win_type:           str   = "captures"
    win_threshold:      int   = 5

    # Players
    num_players:        int   = 2

    # ── derived helpers ───────────────────────────────────────────────────────

    def num_conflict_pairs(self) -> int:
        n = self.num_types
        return n * (n - 1) // 2

    def conflict_outcome(self, type_a: int, type_b: int) -> int:
        """Return raw conflict value for the pair (type_a, type_b).

        Always normalises so i < j before looking up.
        Returns 0/1/2/3 regardless of argument order.
        1 means the *smaller-index* type wins.
        """
        if type_a == type_b:
            return 0
        i, j = (type_a, type_b) if type_a < type_b else (type_b, type_a)
        idx = 0
        for a in range(i):
            idx += self.num_types - 1 - a
        idx += j - i - 1
        if idx < len(self.conflict):
            return self.conflict[idx]
        return 0

    def outcome_for(self, attacker: int, defender: int) -> int:
        """Return +1 if attacker wins, -1 if defender wins, 0 if coexist, None=mutual."""
        raw = self.conflict_outcome(attacker, defender)
        if raw == 0:
            return 0
        if raw == 3:
            return None  # type: ignore
        i = min(attacker, defender)
        if raw == 1:
            winner = i
        else:
            winner = max(attacker, defender)
        return 1 if winner == attacker else -1

    def description_length(self) -> float:
        """Approximate Kolmogorov complexity in bits."""
        bits = 0.0
        bits += math.log2(len(BOARD_SYMMETRIES))
        bits += math.log2(len(NUM_RINGS))
        bits += math.log2(len(NUM_TYPES))
        bits += math.log2(len(ELEMENTS_PER_PLAYER))
        bits += 1.0                                        # food_enabled
        if self.food_enabled:
            bits += math.log2(len(FOOD_INITIAL))
        bits += 5.0                                        # 5 action booleans
        bits += self.num_conflict_pairs() * math.log2(len(CONFLICT_VALUES))
        bits += math.log2(len(WIN_TYPES))
        bits += math.log2(len(WIN_THRESHOLDS))
        bits += math.log2(len(NUM_PLAYERS))
        return bits

    def to_dict(self) -> dict:
        import dataclasses
        d = dataclasses.asdict(self)
        d["conflict"] = list(d["conflict"])
        return d

    @staticmethod
    def from_dict(d: dict) -> "RuleSet":
        d = dict(d)
        d["conflict"] = tuple(d["conflict"])
        return RuleSet(**d)

    def short_id(self) -> str:
        """Compact human-readable ID."""
        return (f"s{self.board_symmetry}r{self.num_rings}"
                f"t{self.num_types}p{self.num_players}"
                f"_{self.win_type[0]}{self.win_threshold}")
