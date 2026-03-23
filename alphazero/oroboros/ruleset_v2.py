"""RuleSet v2: compositional game description.

Each game is a composition of independent sub-specifications:
  Board × Pieces × Actions × Interactions × Turns × Win × Info × Resources

This replaces the flat RuleSet with a structured description that can represent
games far beyond the Organism template.
"""

from __future__ import annotations
import math
from dataclasses import dataclass, field
from typing import Any

from .actions import (
    ActionType, ACTION_CATALOG, ACTION_BY_NAME,
    EFFECTS, TARGET_OWNERS, DIRECTIONS,
)
from .topology import TOPOLOGIES, BOARD_SIZES, SYMMETRIES

# ── Parameter domains ─────────────────────────────────────────────────────────────

NUM_TYPES = [1, 2, 3, 4]
ELEMENTS_PER_PLAYER = [1, 2, 3, 4, 5, 6]
NUM_PLAYERS = [2, 3, 4, 5]
CONFLICT_VALUES = [0, 1, 2, 3]  # coexist, i_wins, j_wins, mutual
TURN_STRUCTURES = ["single", "multi", "action_points"]
ACTIONS_PER_TURN = [1, 2, 3]
WIN_CONDITIONS = ["captures", "population", "elimination", "territory", "score_at_limit"]
WIN_THRESHOLDS = [2, 3, 5, 7]
TURN_LIMITS = [30, 50, 75, 100]  # short games give stronger learning signal
VISIBILITY_TYPES = ["perfect", "fog"]
FOG_RADII = [1, 2, 3]
RESOURCE_SYSTEMS = ["none", "food", "energy"]
RESOURCE_AMOUNTS = [0, 1, 2, 3]
INTERACTION_TRIGGERS = ["adjacency", "move_into", "explicit"]


@dataclass(frozen=True)
class BoardSpec:
    topology:    str  = "ring"     # "ring" | "square" | "hex" | "torus_square" | "torus_hex" | "petersen"
    size:        int  = 4          # ring: num_rings; grid: side_length; hex: radius
    symmetry:    int  = 4          # ring only

    def description_bits(self) -> float:
        bits = math.log2(len(TOPOLOGIES))
        bits += math.log2(len(BOARD_SIZES))
        if self.topology == "ring":
            bits += math.log2(len(SYMMETRIES))
        return bits


@dataclass(frozen=True)
class PieceSpec:
    num_types:          int   = 3
    elements_per_player: int  = 3

    def description_bits(self) -> float:
        return math.log2(len(NUM_TYPES)) + math.log2(len(ELEMENTS_PER_PLAYER))


@dataclass(frozen=True)
class InteractionSpec:
    conflict:  tuple  = (1, 2, 1)  # per pair (i,j) i<j: 0=coexist 1=i_wins 2=j_wins 3=mutual
    trigger:   str    = "adjacency"  # when do conflicts resolve?

    def num_pairs(self, num_types: int) -> int:
        return num_types * (num_types - 1) // 2

    def description_bits(self, num_types: int) -> float:
        n_pairs = self.num_pairs(num_types)
        return n_pairs * math.log2(len(CONFLICT_VALUES)) + math.log2(len(INTERACTION_TRIGGERS))

    def conflict_index(self, num_types: int, i: int, j: int) -> int:
        a, b = min(i, j), max(i, j)
        idx = 0
        for x in range(a):
            idx += num_types - 1 - x
        return idx + b - a - 1

    def outcome_raw(self, num_types: int, type_a: int, type_b: int) -> int:
        if type_a == type_b:
            return 0
        idx = self.conflict_index(num_types, type_a, type_b)
        return self.conflict[idx] if idx < len(self.conflict) else 0

    def outcome_for(self, num_types: int, attacker: int, defender: int) -> str:
        """Returns 'wins', 'loses', 'coexist', or 'mutual'."""
        raw = self.outcome_raw(num_types, attacker, defender)
        if raw == 0:
            return "coexist"
        if raw == 3:
            return "mutual"
        i = min(attacker, defender)
        winner = i if raw == 1 else max(attacker, defender)
        return "wins" if winner == attacker else "loses"


@dataclass(frozen=True)
class TurnSpec:
    structure:       str  = "single"   # "single" | "multi" | "action_points"
    actions_per_turn: int = 1          # for "multi"
    action_points:    int = 1          # for "action_points"

    def description_bits(self) -> float:
        bits = math.log2(len(TURN_STRUCTURES))
        if self.structure == "multi":
            bits += math.log2(len(ACTIONS_PER_TURN))
        elif self.structure == "action_points":
            bits += math.log2(3)  # 1-3 AP
        return bits


@dataclass(frozen=True)
class WinSpec:
    condition:   str  = "captures"
    threshold:   int  = 5
    turn_limit:  int  = 100   # all games have a turn limit for termination

    def description_bits(self) -> float:
        bits = math.log2(len(WIN_CONDITIONS))
        bits += math.log2(len(WIN_THRESHOLDS))
        bits += math.log2(len(TURN_LIMITS))
        return bits


@dataclass(frozen=True)
class InfoSpec:
    visibility:  str  = "perfect"
    fog_radius:  int  = 2

    def description_bits(self) -> float:
        bits = 1.0  # perfect vs fog
        if self.visibility == "fog":
            bits += math.log2(len(FOG_RADII))
        return bits


@dataclass(frozen=True)
class ResourceSpec:
    system:          str  = "none"   # "none" | "food" | "energy"
    initial_amount:  int  = 0
    regen_rate:      int  = 0        # per turn for "energy"

    def description_bits(self) -> float:
        bits = math.log2(len(RESOURCE_SYSTEMS))
        if self.system != "none":
            bits += math.log2(len(RESOURCE_AMOUNTS))
            if self.system == "energy":
                bits += math.log2(3)  # regen rate 0-2
        return bits


@dataclass(frozen=True)
class RuleSetV2:
    """Compositional game description."""
    board:        BoardSpec        = field(default_factory=BoardSpec)
    pieces:       PieceSpec        = field(default_factory=PieceSpec)
    interactions: InteractionSpec  = field(default_factory=InteractionSpec)
    turns:        TurnSpec         = field(default_factory=TurnSpec)
    win:          WinSpec          = field(default_factory=WinSpec)
    info:         InfoSpec         = field(default_factory=InfoSpec)
    resources:    ResourceSpec     = field(default_factory=ResourceSpec)
    num_players:  int              = 2
    # Actions: tuple of ActionType names (looked up from catalog)
    action_names: tuple            = ("move", "capture")

    @property
    def actions(self) -> list[ActionType]:
        return [ACTION_BY_NAME[n] for n in self.action_names if n in ACTION_BY_NAME]

    def description_length(self) -> float:
        bits = 0.0
        bits += self.board.description_bits()
        bits += self.pieces.description_bits()
        bits += self.interactions.description_bits(self.pieces.num_types)
        bits += self.turns.description_bits()
        bits += self.win.description_bits()
        bits += self.info.description_bits()
        bits += self.resources.description_bits()
        bits += math.log2(len(NUM_PLAYERS))
        # Action selection: each action in catalog is on or off
        bits += len(ACTION_CATALOG) * 1.0  # 1 bit per action
        return bits

    def short_id(self) -> str:
        b = self.board
        return (f"{b.topology[:3]}{b.size}"
                f"t{self.pieces.num_types}p{self.num_players}"
                f"_{self.win.condition[0]}{self.win.threshold}")

    def to_dict(self) -> dict:
        import dataclasses
        d = {}
        for name in ["board", "pieces", "interactions", "turns", "win", "info", "resources"]:
            spec = getattr(self, name)
            d[name] = dataclasses.asdict(spec)
            # Convert tuples to lists for JSON
            for k, v in d[name].items():
                if isinstance(v, tuple):
                    d[name][k] = list(v)
        d["num_players"] = self.num_players
        d["action_names"] = list(self.action_names)
        return d

    @staticmethod
    def from_dict(d: dict) -> "RuleSetV2":
        return RuleSetV2(
            board=BoardSpec(**d.get("board", {})),
            pieces=PieceSpec(**d.get("pieces", {})),
            interactions=InteractionSpec(**{
                k: (tuple(v) if k == "conflict" else v)
                for k, v in d.get("interactions", {}).items()
            }),
            turns=TurnSpec(**d.get("turns", {})),
            win=WinSpec(**d.get("win", {})),
            info=InfoSpec(**d.get("info", {})),
            resources=ResourceSpec(**d.get("resources", {})),
            num_players=d.get("num_players", 2),
            action_names=tuple(d.get("action_names", ["move", "capture"])),
        )

    @staticmethod
    def from_v1(old: dict) -> "RuleSetV2":
        """Convert a v1 flat RuleSet dict to v2."""
        action_names = []
        if old.get("can_move"):      action_names.append("move")
        if old.get("can_eat"):       action_names.append("eat")
        if old.get("can_grow"):      action_names.append("grow")
        if old.get("can_capture"):   action_names.append("capture")
        if old.get("can_circulate"): action_names.append("circulate")

        return RuleSetV2(
            board=BoardSpec(
                topology="ring",
                size=old.get("num_rings", 4),
                symmetry=old.get("board_symmetry", 4),
            ),
            pieces=PieceSpec(
                num_types=old.get("num_types", 3),
                elements_per_player=old.get("elements_per_player", 3),
            ),
            interactions=InteractionSpec(
                conflict=tuple(old.get("conflict", (1, 2, 1))),
                trigger="adjacency",
            ),
            turns=TurnSpec(structure="single"),
            win=WinSpec(
                condition=old.get("win_type", "captures"),
                threshold=old.get("win_threshold", 5),
            ),
            info=InfoSpec(visibility="perfect"),
            resources=ResourceSpec(
                system="food" if old.get("food_enabled") else "none",
                initial_amount=old.get("food_initial", 0),
            ),
            num_players=old.get("num_players", 2),
            action_names=tuple(action_names),
        )
