"""Grammar: valid parameter space + sampling + mutation + crossover.

The Grammar defines the search space. Each method returns a RuleSet that
obeys all structural constraints (e.g., conflict tuple length matches num_types).
"""

from __future__ import annotations
import random
from typing import Callable

from .ruleset import (
    RuleSet,
    BOARD_SYMMETRIES, NUM_RINGS, NUM_TYPES, ELEMENTS_PER_PLAYER,
    FOOD_INITIAL, WIN_TYPES, WIN_THRESHOLDS, NUM_PLAYERS, CONFLICT_VALUES,
)


def _random_conflict(num_types: int) -> tuple:
    n_pairs = num_types * (num_types - 1) // 2
    return tuple(random.choice(CONFLICT_VALUES) for _ in range(n_pairs))


def _conflict_for_types(conflict: tuple, new_n: int) -> tuple:
    """Resize conflict tuple to match new_n types, padding or truncating."""
    n_pairs = new_n * (new_n - 1) // 2
    lst = list(conflict)
    while len(lst) < n_pairs:
        lst.append(random.choice(CONFLICT_VALUES))
    return tuple(lst[:n_pairs])


def _validate(rs: RuleSet) -> bool:
    """Return True iff the ruleset is structurally valid and non-degenerate."""
    # Conflict tuple must have exactly the right length
    if len(rs.conflict) != rs.num_conflict_pairs():
        return False
    # Must have at least one action available
    has_action = rs.can_move or rs.can_eat or rs.can_grow or rs.can_capture or rs.can_circulate
    if not has_action:
        return False
    # Food-dependent actions require food
    if not rs.food_enabled and (rs.can_eat or rs.can_grow or rs.can_circulate):
        return False
    # Win threshold must be achievable
    if rs.win_type == "captures":
        # Max captures = all opponent pieces (each player can capture at most this many)
        max_captures = rs.elements_per_player * (rs.num_players - 1)
        if rs.win_threshold > max_captures:
            return False
    elif rs.win_type == "population":
        # Population can only increase via grow; without it, can't exceed starting count
        if not rs.can_grow and rs.win_threshold > rs.elements_per_player:
            return False
    return True


class Grammar:
    """Defines and samples the ruleset search space."""

    # Mutable fields and their domains (for targeted mutation)
    _FIELDS: list[tuple[str, list]] = [
        ("board_symmetry",      BOARD_SYMMETRIES),
        ("num_rings",           NUM_RINGS),
        ("num_types",           NUM_TYPES),
        ("elements_per_player", ELEMENTS_PER_PLAYER),
        ("food_enabled",        [True, False]),
        ("food_initial",        FOOD_INITIAL),
        ("can_move",            [True, False]),
        ("can_eat",             [True, False]),
        ("can_grow",            [True, False]),
        ("can_capture",         [True, False]),
        ("can_circulate",       [True, False]),
        ("win_type",            WIN_TYPES),
        ("win_threshold",       WIN_THRESHOLDS),
        ("num_players",         NUM_PLAYERS),
    ]

    def __init__(
        self,
        max_attempts: int = 50,
        num_players: int | None = None,   # fix if not None
    ):
        self.max_attempts = max_attempts
        self.fixed_num_players = num_players

    def sample(self) -> RuleSet:
        """Sample a uniformly random valid ruleset."""
        for _ in range(self.max_attempts):
            n_types = random.choice(NUM_TYPES)
            food_on = random.choice([True, False])
            rs = RuleSet(
                board_symmetry=random.choice(BOARD_SYMMETRIES),
                num_rings=random.choice(NUM_RINGS),
                num_types=n_types,
                elements_per_player=random.choice(ELEMENTS_PER_PLAYER),
                food_enabled=food_on,
                food_initial=random.choice(FOOD_INITIAL) if food_on else 0,
                can_move=True,                      # always keep move
                can_eat=food_on and random.choice([True, False]),
                can_grow=food_on and random.choice([True, False]),
                can_capture=random.choice([True, False]),
                can_circulate=food_on and random.choice([True, False]),
                conflict=_random_conflict(n_types),
                win_type=random.choice(WIN_TYPES),
                win_threshold=random.choice(WIN_THRESHOLDS),
                num_players=self.fixed_num_players or random.choice(NUM_PLAYERS),
            )
            if _validate(rs):
                return rs
        # Fallback: return Organism-like ruleset
        return self.organism_like()

    def _build_mutant(self, rs: RuleSet, fname: str, new_val) -> RuleSet | None:
        """Build a mutant RuleSet by setting one field. Returns None if invalid."""
        kwargs = rs.to_dict()
        kwargs["conflict"] = tuple(kwargs["conflict"])
        kwargs[fname] = new_val

        if fname == "num_types":
            kwargs["conflict"] = _conflict_for_types(rs.conflict, new_val)
        if fname == "food_enabled" and not new_val:
            kwargs["can_eat"] = False
            kwargs["can_grow"] = False
            kwargs["can_circulate"] = False

        candidate = RuleSet(**kwargs)
        return candidate if _validate(candidate) else None

    def mutate(
        self,
        rs: RuleSet,
        field: str | None = None,
        field_weights: dict[str, float] | None = None,
        value_weights: dict[str, dict] | None = None,
    ) -> RuleSet:
        """Mutate one field.  Retries until the result is valid.

        Args:
            field_weights: Optional bias for field selection — {field_name: weight}.
            value_weights: Optional bias for value selection — {field_name: {value: weight}}.
        """
        for _ in range(self.max_attempts):
            if field is None:
                fname, domain = self._pick_field(field_weights)
            else:
                fname = field
                domain = dict(self._FIELDS)[fname]

            current = getattr(rs, fname)
            choices = [v for v in domain if v != current]
            if not choices:
                continue
            new_val = self._pick_value(fname, choices, value_weights)

            candidate = self._build_mutant(rs, fname, new_val)
            if candidate is not None:
                return candidate
        return rs

    def mutate_tracked(
        self,
        rs: RuleSet,
        field_weights: dict[str, float] | None = None,
        value_weights: dict[str, dict] | None = None,
    ) -> tuple[RuleSet, str, object, object]:
        """Like mutate(), but returns (child, field_name, old_value, new_value)."""
        for _ in range(self.max_attempts):
            fname, domain = self._pick_field(field_weights)
            current = getattr(rs, fname)
            choices = [v for v in domain if v != current]
            if not choices:
                continue
            new_val = self._pick_value(fname, choices, value_weights)

            candidate = self._build_mutant(rs, fname, new_val)
            if candidate is not None:
                return candidate, fname, current, new_val
        return rs, "", None, None  # unchanged

    def mutate_to(self, rs: RuleSet, field: str, value) -> RuleSet | None:
        """Deterministically set one field to a specific value. Returns None if invalid."""
        return self._build_mutant(rs, field, value)

    def _pick_field(self, weights: dict[str, float] | None) -> tuple[str, list]:
        """Select a mutation field, optionally biased by weights."""
        if weights is None:
            return random.choice(self._FIELDS)
        fields = self._FIELDS
        w = [max(weights.get(f, 1.0), 0.05) for f, _ in fields]
        total = sum(w)
        r = random.random() * total
        cumulative = 0.0
        for i, (fname, domain) in enumerate(fields):
            cumulative += w[i]
            if r <= cumulative:
                return fname, domain
        return fields[-1]

    def _pick_value(self, fname: str, choices: list, weights: dict[str, dict] | None) -> object:
        """Select a mutation value, optionally biased by value weights."""
        if weights is None or fname not in weights:
            return random.choice(choices)
        vw = weights[fname]
        w = [max(vw.get(v, 1.0), 0.1) for v in choices]
        total = sum(w)
        r = random.random() * total
        cumulative = 0.0
        for i, v in enumerate(choices):
            cumulative += w[i]
            if r <= cumulative:
                return v
        return choices[-1]

    def crossover(self, rs1: RuleSet, rs2: RuleSet) -> RuleSet:
        """Uniform crossover, retrying until valid."""
        for _ in range(self.max_attempts):
            kwargs: dict = {}
            for fname, _ in self._FIELDS:
                kwargs[fname] = random.choice([getattr(rs1, fname), getattr(rs2, fname)])
            # Reconcile conflict length with chosen num_types
            n = kwargs["num_types"]
            c1 = _conflict_for_types(rs1.conflict, n)
            c2 = _conflict_for_types(rs2.conflict, n)
            kwargs["conflict"] = tuple(
                random.choice([a, b]) for a, b in zip(c1, c2)
            )
            candidate = RuleSet(**kwargs)
            if _validate(candidate):
                return candidate
        return self.mutate(rs1)

    # ── named presets ─────────────────────────────────────────────────────────

    def organism_like(self) -> RuleSet:
        """Closest to the full Organism game (5-ring, 5-sym, 3 types, 5 players)."""
        return RuleSet(
            board_symmetry=5, num_rings=5, num_types=3, elements_per_player=3,
            food_enabled=True, food_initial=1,
            can_move=True, can_eat=True, can_grow=True,
            can_capture=False, can_circulate=True,
            conflict=(1, 2, 1),
            win_type="captures", win_threshold=5,
            num_players=5,
        )

    def minimal_go_like(self) -> RuleSet:
        """Minimal placement + capture game (no food, 1 type, capture win). 2p × 4e → threshold=3."""
        return RuleSet(
            board_symmetry=4, num_rings=4, num_types=1, elements_per_player=4,
            food_enabled=False, food_initial=0,
            can_move=True, can_eat=False, can_grow=False,
            can_capture=True, can_circulate=False,
            conflict=(3,),           # same type → mutual destruction
            win_type="captures", win_threshold=3,
            num_players=2,
        )

    def heterarchy_minimal(self) -> RuleSet:
        """Minimal 3-type heterarchy, no food, direct capture. 2 players × 3 elements → threshold=3."""
        return RuleSet(
            board_symmetry=4, num_rings=4, num_types=3, elements_per_player=3,
            food_enabled=False, food_initial=0,
            can_move=True, can_eat=False, can_grow=False,
            can_capture=True, can_circulate=False,
            conflict=(1, 2, 1),
            win_type="captures", win_threshold=3,
            num_players=2,
        )
