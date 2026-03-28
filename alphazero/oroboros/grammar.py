"""Grammar: generate, mutate, and crossover rule-programs.

Creates random valid games where rules are (predicate, effect) AST pairs
composed from primitives. No predefined action catalog.
"""

from __future__ import annotations
import random
import math

from .schema import *
from .interpreter import predicate_max_range


# ── Domains ───────────────────────────────────────────────────────────────────

from .tiling import BASE_TYPES, TILING_SIZES, SYMMETRIES, BOUNDARIES, MODIFIERS, MOD_PARAMS

PIECE_TYPES = [1, 2, 3, 4]
PLAYER_COUNTS = [2, 3]
RESOURCE_SLOTS = [0, 1, 2]
GLOBAL_COUNTERS = [2, 3, 4]
CONFLICT_VALUES = [0, 1, 2, 3]
PLACEMENTS = ["spread", "cluster", "random"]
TURN_LIMITS = [30, 50, 75, 100]
ACTIONS_PER_TURN = [1, 2, 3]

MAX_RULES = 5
MIN_RULES = 1
MAX_PRED_DEPTH = 2
MAX_EFF_DEPTH = 2


class Grammar:
    """Generate and mutate rule-program games."""

    def __init__(self, max_attempts: int = 100, fixed_players: int | None = None):
        self.max_attempts = max_attempts
        self.fixed_players = fixed_players

    def sample(self) -> RuleSetV3:
        """Generate a random valid game."""
        for _ in range(self.max_attempts):
            schema = self._random_schema()
            conflict = self._random_conflict(schema.num_piece_types)
            n_rules = random.randint(MIN_RULES, MAX_RULES)
            rules = tuple(self._random_rule(schema) for _ in range(n_rules))
            interactions = self._random_interactions(schema)
            terminations = self._random_terminations(schema)

            rs = RuleSetV3(
                schema=schema,
                conflict_table=conflict,
                rules=rules,
                interactions=interactions,
                terminations=terminations,
                actions_per_turn=random.choice(ACTIONS_PER_TURN),
                turn_limit=random.choice(TURN_LIMITS),
                placement=random.choice(PLACEMENTS),
            )
            if validate_coverage(rs) and self._quick_viable(rs):
                return rs
        return self._fallback()

    def mutate(self, rs: RuleSetV3, gentle: bool = False) -> RuleSetV3:
        """Mutate one component. If gentle=True, prefer small tweaks."""
        if gentle:
            # Gentle: only tweak conflict values or turn structure (small changes)
            ops = [
                self._mut_conflict, self._mut_conflict, self._mut_turns,
                self._mut_rule_pred,
            ]
        else:
            # Normal: full range of mutations, weighted toward smaller changes
            ops = [
                self._mut_conflict,     # small: flip one conflict value
                self._mut_conflict,     # (double weight)
                self._mut_turns,        # small: change actions per turn
                self._mut_rule_pred,    # medium: replace one rule predicate
                self._mut_rule_eff,     # medium: replace one rule effect
                self._mut_termination,  # medium: change win condition
                self._mut_schema,       # large: change topology/types
                self._mut_add_rule,     # large: add a rule
                self._mut_remove_rule,  # large: remove a rule
            ]
        for _ in range(self.max_attempts):
            candidate = random.choice(ops)(rs)
            if validate_coverage(candidate) and self._quick_viable(candidate):
                return candidate
        return rs

    def crossover(self, a: RuleSetV3, b: RuleSetV3) -> RuleSetV3:
        """Mix rules from two parents."""
        schema = random.choice([a.schema, b.schema])
        # Resize conflict table for chosen schema
        T = schema.num_piece_types
        n_pairs = T * (T - 1) // 2
        ca = list(a.conflict_table)
        cb = list(b.conflict_table)
        while len(ca) < n_pairs: ca.append(random.choice(CONFLICT_VALUES))
        while len(cb) < n_pairs: cb.append(random.choice(CONFLICT_VALUES))
        conflict = tuple(random.choice([ca[i], cb[i]]) for i in range(n_pairs))

        # Mix rules
        all_rules = list(a.rules) + list(b.rules)
        n = random.randint(MIN_RULES, min(len(all_rules), MAX_RULES))
        rules = tuple(random.sample(all_rules, n))

        for _ in range(self.max_attempts):
            candidate = RuleSetV3(
                schema=schema, conflict_table=conflict, rules=rules,
                interactions=random.choice([a.interactions, b.interactions]),
                terminations=random.choice([a.terminations, b.terminations]),
                actions_per_turn=random.choice([a.actions_per_turn, b.actions_per_turn]),
                turn_limit=random.choice([a.turn_limit, b.turn_limit]),
                placement=random.choice([a.placement, b.placement]),
            )
            if validate_coverage(candidate):
                return candidate
        return self.mutate(a)

    # ── Random generation ─────────────────────────────────────────────────────

    def _random_schema(self) -> StateSchema:
        np = self.fixed_players or random.choice(PLAYER_COUNTS)
        modifier = random.choice(MODIFIERS)
        return StateSchema(
            topology_type=random.choice(BASE_TYPES),
            topology_size=random.choice(TILING_SIZES),
            topology_symmetry=random.choice(SYMMETRIES),
            topology_boundary=random.choice(BOUNDARIES),
            topology_modifier=modifier,
            topology_mod_param=random.choice(MOD_PARAMS) if modifier != "none" else 0,
            num_piece_types=random.choice(PIECE_TYPES),
            num_players=np,
            num_resource_slots=random.choice(RESOURCE_SLOTS),
            num_global_counters=max(np, random.choice(GLOBAL_COUNTERS)),
        )

    def _random_conflict(self, num_types: int) -> tuple:
        n_pairs = num_types * (num_types - 1) // 2
        return tuple(random.choice(CONFLICT_VALUES) for _ in range(n_pairs))

    def _random_rule(self, schema: StateSchema) -> Rule:
        """Generate a rule with a spatial constraint + random predicate/effect."""
        # Always include a spatial constraint to bound branching
        spatial = random.choice([
            (P_ADJACENT,),
            (P_DISTANCE_LEQ, 2),
        ])
        extra_pred = self._random_predicate(schema, depth=0)
        predicate = (P_AND, spatial, extra_pred)
        effect = self._random_effect(schema, depth=0)
        requires_piece = random.random() < 0.75
        return Rule(predicate=predicate, effect=effect, requires_piece=requires_piece)

    def _random_predicate(self, schema: StateSchema, depth: int) -> tuple:
        if depth >= MAX_PRED_DEPTH or random.random() < 0.5:
            return self._random_leaf_pred(schema)
        op = random.choice([P_AND, P_OR])
        return (op,
                self._random_predicate(schema, depth + 1),
                self._random_predicate(schema, depth + 1))

    def _random_leaf_pred(self, schema: StateSchema) -> tuple:
        which = random.choice([SRC, TGT])
        pool = [
            (P_EMPTY, TGT),
            (P_OCCUPIED, TGT),
            (P_OWNER_IS, SRC, SELF),
            (P_OWNER_IS, TGT, ENEMY),
            (P_OWNER_IS, TGT, SELF),
            (P_TRUE,),
        ]
        if schema.num_piece_types > 1:
            pool.append((P_TYPE_IS, which, random.randrange(schema.num_piece_types)))
            pool.append((P_TYPE_BEATS,))
            pool.append((P_SAME_TYPE,))
            pool.append((P_DIFF_TYPE,))
        for slot in range(schema.num_resource_slots):
            pool.append((P_RESOURCE_GEQ, which, slot, random.choice([1, 2, 3])))
        for gi in range(schema.num_global_counters):
            pool.append((P_GLOBAL_GEQ, gi, random.choice([2, 3, 5])))
        return random.choice(pool)

    def _random_effect(self, schema: StateSchema, depth: int) -> tuple:
        if depth >= MAX_EFF_DEPTH or random.random() < 0.6:
            return self._random_leaf_eff(schema)
        return (E_SEQ,
                self._random_leaf_eff(schema),
                self._random_effect(schema, depth + 1))

    def _random_leaf_eff(self, schema: StateSchema) -> tuple:
        pool = [
            (E_REMOVE_PIECE, TGT),
            (E_REMOVE_PIECE, SRC),
            (E_MOVE_PIECE, SRC, TGT),
            (E_PLACE_PIECE, TGT, SELF, random.randrange(max(schema.num_piece_types, 1))),
            (E_PLACE_PIECE, TGT, SELF, COPY_SRC_TYPE),
            (E_SWAP_PIECES,),
            (E_PUSH_PIECE, TGT, SRC),
        ]
        for slot in range(schema.num_resource_slots):
            pool.extend([
                (E_ADD_RESOURCE, SRC, slot, -1),
                (E_ADD_RESOURCE, TGT, slot, 1),
                (E_TRANSFER_RES, SRC, TGT, slot, 1),
                (E_COLLECT_RES, SRC, slot),
            ])
        for gi in range(schema.num_global_counters):
            pool.append((E_INC_COUNTER, gi, 1))
        return random.choice(pool)

    def _random_interactions(self, schema: StateSchema) -> tuple:
        if random.random() < 0.4:
            return ()  # no automatic interactions
        # Type-beats interaction (like the classic conflict table)
        if schema.num_piece_types > 1:
            return (InteractionRule(
                trigger="adjacency",
                predicate=(P_TYPE_BEATS,),
                effect=(E_SEQ,
                        (E_REMOVE_PIECE, TGT),
                        (E_INC_COUNTER, 0, 1)),  # increment score
            ),)
        return ()

    def _random_terminations(self, schema: StateSchema) -> tuple:
        pool = []
        # Counter threshold
        for gi in range(min(schema.num_global_counters, schema.num_players)):
            pool.append(TerminationSpec(
                predicate=(T_COUNTER_GEQ, gi, random.choice([3, 5, 7])),
                winner_mode=W_TRIGGERING_PLAYER,
            ))
        # Elimination
        pool.append(TerminationSpec(
            predicate=(T_ONE_PLAYER_LEFT,),
            winner_mode=W_LAST_STANDING,
        ))
        # Population
        pool.append(TerminationSpec(
            predicate=(T_PIECE_COUNT_GEQ, SELF, random.choice([5, 7, 10])),
            winner_mode=W_TRIGGERING_PLAYER,
        ))
        # Pick 1-2 conditions
        chosen = random.sample(pool, min(2, len(pool)))
        # Always add turn limit fallback
        chosen.append(TerminationSpec(
            predicate=(T_ROUND_GEQ, 100),
            winner_mode=W_HIGHEST_COUNTER,
            winner_arg=0,
        ))
        return tuple(chosen)

    def _quick_viable(self, rs: RuleSetV3) -> bool:
        """Fast sanity check."""
        if not rs.rules:
            return False
        # At least one rule must modify pieces
        for rule in rs.rules:
            refs = fields_referenced(rule.effect)
            if "owner" in refs or "piece_type" in refs:
                return True
        return False

    def _fallback(self) -> RuleSetV3:
        """Minimal valid game: move + capture on hex."""
        schema = StateSchema(
            topology_type="hex", topology_size=3,
            num_piece_types=2, num_players=2,
            num_resource_slots=0, num_global_counters=2,
        )
        return RuleSetV3(
            schema=schema,
            conflict_table=(1,),
            rules=(
                # "move": own piece at src, empty tgt, adjacent → relocate
                Rule(
                    predicate=(P_AND, (P_ADJACENT,),
                               (P_AND, (P_OWNER_IS, SRC, SELF), (P_EMPTY, TGT))),
                    effect=(E_MOVE_PIECE, SRC, TGT),
                    requires_piece=True,
                ),
                # "capture": own piece at src, enemy tgt, adjacent, type beats → remove + score
                Rule(
                    predicate=(P_AND, (P_ADJACENT,),
                               (P_AND, (P_OWNER_IS, TGT, ENEMY), (P_TYPE_BEATS,))),
                    effect=(E_SEQ, (E_REMOVE_PIECE, TGT), (E_INC_COUNTER, 0, 1)),
                    requires_piece=True,
                ),
            ),
            terminations=(
                TerminationSpec(predicate=(T_COUNTER_GEQ, 0, 3), winner_mode=W_TRIGGERING_PLAYER),
                TerminationSpec(predicate=(T_ROUND_GEQ, 50), winner_mode=W_HIGHEST_COUNTER, winner_arg=0),
            ),
            turn_limit=50,
        )

    # ── Mutation operators ────────────────────────────────────────────────────

    def _mut_schema(self, rs: RuleSetV3) -> RuleSetV3:
        s = rs.schema
        field = random.choice(["topology_type", "topology_size", "topology_boundary",
                                "topology_modifier", "num_piece_types",
                                "num_resource_slots", "num_global_counters"])
        kwargs = {
            "topology_type": s.topology_type,
            "topology_size": s.topology_size,
            "topology_symmetry": s.topology_symmetry,
            "topology_boundary": s.topology_boundary,
            "topology_modifier": s.topology_modifier,
            "topology_mod_param": s.topology_mod_param,
            "num_piece_types": s.num_piece_types,
            "num_players": s.num_players,
            "num_resource_slots": s.num_resource_slots,
            "num_global_counters": s.num_global_counters,
        }
        if field == "topology_type":
            kwargs[field] = random.choice([t for t in BASE_TYPES if t != s.topology_type])
        elif field == "topology_size":
            kwargs[field] = random.choice([sz for sz in TILING_SIZES if sz != s.topology_size])
        elif field == "topology_boundary":
            kwargs[field] = "torus" if s.topology_boundary == "finite" else "finite"
        elif field == "topology_modifier":
            mod = random.choice([m for m in MODIFIERS if m != s.topology_modifier])
            kwargs["topology_modifier"] = mod
            kwargs["topology_mod_param"] = random.choice(MOD_PARAMS) if mod != "none" else 0
        elif field == "num_piece_types":
            new_t = random.choice([t for t in PIECE_TYPES if t != s.num_piece_types])
            kwargs[field] = new_t
            # Resize conflict table
            n_pairs = new_t * (new_t - 1) // 2
            ct = list(rs.conflict_table)
            while len(ct) < n_pairs: ct.append(random.choice(CONFLICT_VALUES))
            return RuleSetV3(**{**_rs_dict(rs), "schema": StateSchema(**kwargs),
                                "conflict_table": tuple(ct[:n_pairs])})
        elif field == "num_resource_slots":
            kwargs[field] = random.choice([r for r in RESOURCE_SLOTS if r != s.num_resource_slots])
        elif field == "num_global_counters":
            kwargs[field] = random.choice([g for g in GLOBAL_COUNTERS if g != s.num_global_counters])
        return RuleSetV3(**{**_rs_dict(rs), "schema": StateSchema(**kwargs)})

    def _mut_conflict(self, rs: RuleSetV3) -> RuleSetV3:
        ct = list(rs.conflict_table)
        if ct:
            idx = random.randrange(len(ct))
            ct[idx] = random.choice([v for v in CONFLICT_VALUES if v != ct[idx]])
        return RuleSetV3(**{**_rs_dict(rs), "conflict_table": tuple(ct)})

    def _mut_rule_pred(self, rs: RuleSetV3) -> RuleSetV3:
        if not rs.rules:
            return rs
        rules = list(rs.rules)
        idx = random.randrange(len(rules))
        old = rules[idx]
        new_pred = (P_AND,
                    random.choice([(P_ADJACENT,), (P_DISTANCE_LEQ, 2)]),
                    self._random_predicate(rs.schema, 0))
        rules[idx] = Rule(new_pred, old.effect, old.requires_piece)
        return RuleSetV3(**{**_rs_dict(rs), "rules": tuple(rules)})

    def _mut_rule_eff(self, rs: RuleSetV3) -> RuleSetV3:
        if not rs.rules:
            return rs
        rules = list(rs.rules)
        idx = random.randrange(len(rules))
        old = rules[idx]
        new_eff = self._random_effect(rs.schema, 0)
        rules[idx] = Rule(old.predicate, new_eff, old.requires_piece)
        return RuleSetV3(**{**_rs_dict(rs), "rules": tuple(rules)})

    def _mut_add_rule(self, rs: RuleSetV3) -> RuleSetV3:
        if len(rs.rules) >= MAX_RULES:
            return rs
        rules = list(rs.rules) + [self._random_rule(rs.schema)]
        return RuleSetV3(**{**_rs_dict(rs), "rules": tuple(rules)})

    def _mut_remove_rule(self, rs: RuleSetV3) -> RuleSetV3:
        if len(rs.rules) <= MIN_RULES:
            return rs
        rules = list(rs.rules)
        rules.pop(random.randrange(len(rules)))
        return RuleSetV3(**{**_rs_dict(rs), "rules": tuple(rules)})

    def _mut_termination(self, rs: RuleSetV3) -> RuleSetV3:
        new_terms = self._random_terminations(rs.schema)
        return RuleSetV3(**{**_rs_dict(rs), "terminations": new_terms})

    def _mut_turns(self, rs: RuleSetV3) -> RuleSetV3:
        new_apt = random.choice([a for a in ACTIONS_PER_TURN if a != rs.actions_per_turn])
        return RuleSetV3(**{**_rs_dict(rs), "actions_per_turn": new_apt})


def _rs_dict(rs: RuleSetV3) -> dict:
    return {
        "schema": rs.schema,
        "conflict_table": rs.conflict_table,
        "rules": rs.rules,
        "interactions": rs.interactions,
        "terminations": rs.terminations,
        "actions_per_turn": rs.actions_per_turn,
        "turn_limit": rs.turn_limit,
        "placement": rs.placement,
    }
