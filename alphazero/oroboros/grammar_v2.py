"""Grammar v2: sampling, mutation, and crossover over the compositional game space.

Samples from: topology × piece types × action types × interactions × turn structure
              × win conditions × information × resources
"""

from __future__ import annotations
import random
import math
from .ruleset_v2 import (
    RuleSetV2, BoardSpec, PieceSpec, InteractionSpec, TurnSpec, WinSpec,
    InfoSpec, ResourceSpec,
    NUM_TYPES, ELEMENTS_PER_PLAYER, NUM_PLAYERS, CONFLICT_VALUES,
    TURN_STRUCTURES, ACTIONS_PER_TURN, WIN_CONDITIONS, WIN_THRESHOLDS,
    TURN_LIMITS, VISIBILITY_TYPES, FOG_RADII, RESOURCE_SYSTEMS, RESOURCE_AMOUNTS,
    INTERACTION_TRIGGERS,
)
from .topology import TOPOLOGIES, BOARD_SIZES, SYMMETRIES
from .actions import ACTION_CATALOG


def _random_conflict(num_types: int) -> tuple:
    n_pairs = num_types * (num_types - 1) // 2
    return tuple(random.choice(CONFLICT_VALUES) for _ in range(n_pairs))


def _resize_conflict(conflict: tuple, new_n: int) -> tuple:
    n_pairs = new_n * (new_n - 1) // 2
    lst = list(conflict)
    while len(lst) < n_pairs:
        lst.append(random.choice(CONFLICT_VALUES))
    return tuple(lst[:n_pairs])


def _random_actions() -> tuple:
    """Sample a random subset of action types (2-4 is the sweet spot).

    Too few (1) = trivial. Too many (6+) = branching explodes and AZ can't learn.
    """
    # Always include move or place as the base mobility action
    base = [random.choice(["move", "place"])]
    # Pick 1-3 additional actions (biased toward 1-2)
    others = [at.name for at in ACTION_CATALOG if at.name not in base]
    random.shuffle(others)
    n_extra = random.choice([1, 1, 2, 2, 2, 3])  # median 2
    base.extend(others[:n_extra])
    return tuple(base)


def _validate(rs: RuleSetV2) -> bool:
    """Structural validity check."""
    # Must have at least one action
    if not rs.action_names:
        return False
    # Conflict tuple must match num_types
    expected_pairs = rs.pieces.num_types * (rs.pieces.num_types - 1) // 2
    if len(rs.interactions.conflict) != expected_pairs:
        return False
    # Resource-dependent actions need resources
    has_resources = rs.resources.system != "none"
    for aname in rs.action_names:
        from .actions import ACTION_BY_NAME
        at = ACTION_BY_NAME.get(aname)
        if at and at.resource_cost > 0 and not has_resources:
            return False
    # Win threshold achievability
    if rs.win.condition == "captures":
        max_cap = rs.pieces.elements_per_player * (rs.num_players - 1)
        if rs.win.threshold > max_cap:
            return False
    elif rs.win.condition == "population":
        has_spawn = any(n in ("grow", "place") for n in rs.action_names)
        if not has_spawn and rs.win.threshold > rs.pieces.elements_per_player:
            return False
    # 1-type games don't need conflict
    if rs.pieces.num_types == 1 and all(c == 0 for c in rs.interactions.conflict):
        # Fine — 1 type means no pairwise conflicts needed
        pass
    return True


class GrammarV2:
    """Sample and mutate compositional rulesets."""

    def __init__(self, max_attempts: int = 50, fixed_players: int | None = None):
        self.max_attempts = max_attempts
        self.fixed_players = fixed_players

    def sample(self) -> RuleSetV2:
        """Uniformly random valid ruleset from the full compositional space."""
        for _ in range(self.max_attempts):
            num_types = random.choice(NUM_TYPES)
            resource_sys = random.choice(RESOURCE_SYSTEMS)
            has_resources = resource_sys != "none"
            topology = random.choice(TOPOLOGIES)
            win_cond = random.choice(WIN_CONDITIONS)

            rs = RuleSetV2(
                board=BoardSpec(
                    topology=topology,
                    size=random.choice(BOARD_SIZES),
                    symmetry=random.choice(SYMMETRIES) if topology == "ring" else 4,
                ),
                pieces=PieceSpec(
                    num_types=num_types,
                    elements_per_player=random.choice(ELEMENTS_PER_PLAYER),
                ),
                interactions=InteractionSpec(
                    conflict=_random_conflict(num_types),
                    trigger=random.choice(INTERACTION_TRIGGERS),
                ),
                turns=TurnSpec(
                    structure=random.choice(TURN_STRUCTURES),
                    actions_per_turn=random.choice(ACTIONS_PER_TURN),
                    action_points=random.choice([1, 2, 3]),
                ),
                win=WinSpec(
                    condition=win_cond,
                    threshold=random.choice(WIN_THRESHOLDS),
                    turn_limit=random.choice(TURN_LIMITS),
                ),
                info=InfoSpec(
                    visibility=random.choice(VISIBILITY_TYPES),
                    fog_radius=random.choice(FOG_RADII),
                ),
                resources=ResourceSpec(
                    system=resource_sys,
                    initial_amount=random.choice(RESOURCE_AMOUNTS) if has_resources else 0,
                    regen_rate=random.choice([0, 1, 2]) if resource_sys == "energy" else 0,
                ),
                num_players=self.fixed_players or random.choice(NUM_PLAYERS),
                action_names=_random_actions(),
            )
            if _validate(rs):
                return rs
        # Fallback
        return self._fallback()

    def _fallback(self) -> RuleSetV2:
        return RuleSetV2(
            board=BoardSpec(topology="square", size=5),
            pieces=PieceSpec(num_types=2, elements_per_player=3),
            interactions=InteractionSpec(conflict=(1,), trigger="adjacency"),
            turns=TurnSpec(structure="single"),
            win=WinSpec(condition="captures", threshold=3),
            resources=ResourceSpec(system="none"),
            num_players=2,
            action_names=("move", "capture"),
        )

    def mutate(self, rs: RuleSetV2) -> RuleSetV2:
        """Mutate one sub-spec or one field within a sub-spec."""
        mutations = [
            self._mutate_board,
            self._mutate_pieces,
            self._mutate_interactions,
            self._mutate_turns,
            self._mutate_win,
            self._mutate_info,
            self._mutate_resources,
            self._mutate_actions,
            self._mutate_players,
        ]
        for _ in range(self.max_attempts):
            fn = random.choice(mutations)
            candidate = fn(rs)
            if _validate(candidate):
                return candidate
        return rs

    def crossover(self, a: RuleSetV2, b: RuleSetV2) -> RuleSetV2:
        """Mix sub-specs from two parents."""
        for _ in range(self.max_attempts):
            pick = lambda: random.choice([True, False])
            num_types = random.choice([a.pieces.num_types, b.pieces.num_types])
            c_a = _resize_conflict(a.interactions.conflict, num_types)
            c_b = _resize_conflict(b.interactions.conflict, num_types)
            conflict = tuple(random.choice([x, y]) for x, y in zip(c_a, c_b))

            candidate = RuleSetV2(
                board=a.board if pick() else b.board,
                pieces=PieceSpec(
                    num_types=num_types,
                    elements_per_player=random.choice([
                        a.pieces.elements_per_player,
                        b.pieces.elements_per_player]),
                ),
                interactions=InteractionSpec(
                    conflict=conflict,
                    trigger=random.choice([a.interactions.trigger, b.interactions.trigger]),
                ),
                turns=a.turns if pick() else b.turns,
                win=a.win if pick() else b.win,
                info=a.info if pick() else b.info,
                resources=a.resources if pick() else b.resources,
                num_players=self.fixed_players or random.choice([a.num_players, b.num_players]),
                action_names=a.action_names if pick() else b.action_names,
            )
            if _validate(candidate):
                return candidate
        return self.mutate(a)

    # ── Sub-spec mutations ────────────────────────────────────────────────────────

    def _mutate_board(self, rs: RuleSetV2) -> RuleSetV2:
        field = random.choice(["topology", "size", "symmetry"])
        b = rs.board
        if field == "topology":
            new = random.choice([t for t in TOPOLOGIES if t != b.topology])
            return RuleSetV2(**{**_asdict(rs), "board": BoardSpec(new, b.size, b.symmetry)})
        elif field == "size":
            new = random.choice([s for s in BOARD_SIZES if s != b.size])
            return RuleSetV2(**{**_asdict(rs), "board": BoardSpec(b.topology, new, b.symmetry)})
        else:
            new = random.choice([s for s in SYMMETRIES if s != b.symmetry])
            return RuleSetV2(**{**_asdict(rs), "board": BoardSpec(b.topology, b.size, new)})

    def _mutate_pieces(self, rs: RuleSetV2) -> RuleSetV2:
        p = rs.pieces
        if random.random() < 0.5:
            new_t = random.choice([t for t in NUM_TYPES if t != p.num_types])
            new_conflict = _resize_conflict(rs.interactions.conflict, new_t)
            d = _asdict(rs)
            d["pieces"] = PieceSpec(new_t, p.elements_per_player)
            d["interactions"] = InteractionSpec(new_conflict, rs.interactions.trigger)
            return RuleSetV2(**d)
        else:
            new_e = random.choice([e for e in ELEMENTS_PER_PLAYER if e != p.elements_per_player])
            return RuleSetV2(**{**_asdict(rs), "pieces": PieceSpec(p.num_types, new_e)})

    def _mutate_interactions(self, rs: RuleSetV2) -> RuleSetV2:
        i = rs.interactions
        if random.random() < 0.7:
            # Flip one conflict value
            conflict = list(i.conflict)
            if conflict:
                idx = random.randrange(len(conflict))
                choices = [v for v in CONFLICT_VALUES if v != conflict[idx]]
                if choices:
                    conflict[idx] = random.choice(choices)
            return RuleSetV2(**{**_asdict(rs), "interactions": InteractionSpec(tuple(conflict), i.trigger)})
        else:
            new_t = random.choice([t for t in INTERACTION_TRIGGERS if t != i.trigger])
            return RuleSetV2(**{**_asdict(rs), "interactions": InteractionSpec(i.conflict, new_t)})

    def _mutate_turns(self, rs: RuleSetV2) -> RuleSetV2:
        t = rs.turns
        new_s = random.choice([s for s in TURN_STRUCTURES if s != t.structure])
        return RuleSetV2(**{**_asdict(rs), "turns": TurnSpec(new_s, t.actions_per_turn, t.action_points)})

    def _mutate_win(self, rs: RuleSetV2) -> RuleSetV2:
        w = rs.win
        if random.random() < 0.5:
            new_c = random.choice([c for c in WIN_CONDITIONS if c != w.condition])
            tl = random.choice(TURN_LIMITS)
            return RuleSetV2(**{**_asdict(rs), "win": WinSpec(new_c, w.threshold, tl)})
        else:
            new_t = random.choice([t for t in WIN_THRESHOLDS if t != w.threshold])
            return RuleSetV2(**{**_asdict(rs), "win": WinSpec(w.condition, new_t, w.turn_limit)})

    def _mutate_info(self, rs: RuleSetV2) -> RuleSetV2:
        i = rs.info
        if i.visibility == "perfect":
            return RuleSetV2(**{**_asdict(rs), "info": InfoSpec("fog", random.choice(FOG_RADII))})
        else:
            return RuleSetV2(**{**_asdict(rs), "info": InfoSpec("perfect", 2)})

    def _mutate_resources(self, rs: RuleSetV2) -> RuleSetV2:
        r = rs.resources
        new_sys = random.choice([s for s in RESOURCE_SYSTEMS if s != r.system])
        has = new_sys != "none"
        return RuleSetV2(**{**_asdict(rs), "resources": ResourceSpec(
            new_sys,
            random.choice(RESOURCE_AMOUNTS) if has else 0,
            random.choice([0, 1, 2]) if new_sys == "energy" else 0,
        )})

    def _mutate_actions(self, rs: RuleSetV2) -> RuleSetV2:
        """Add or remove an action type."""
        names = list(rs.action_names)
        if random.random() < 0.5 and len(names) > 1:
            # Remove random action
            names.pop(random.randrange(len(names)))
        else:
            # Add random action not already present
            available = [a.name for a in ACTION_CATALOG if a.name not in names]
            if available:
                names.append(random.choice(available))
        return RuleSetV2(**{**_asdict(rs), "action_names": tuple(names)})

    def _mutate_players(self, rs: RuleSetV2) -> RuleSetV2:
        if self.fixed_players:
            return rs
        new_p = random.choice([p for p in NUM_PLAYERS if p != rs.num_players])
        return RuleSetV2(**{**_asdict(rs), "num_players": new_p})


def _asdict(rs: RuleSetV2) -> dict:
    """Helper to spread a RuleSetV2 into kwargs for reconstruction."""
    return {
        "board": rs.board,
        "pieces": rs.pieces,
        "interactions": rs.interactions,
        "turns": rs.turns,
        "win": rs.win,
        "info": rs.info,
        "resources": rs.resources,
        "num_players": rs.num_players,
        "action_names": rs.action_names,
    }
