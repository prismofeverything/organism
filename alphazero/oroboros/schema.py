"""Universal game encoding: rules as programs over a state schema.

A game is (StateSchema, conflict_table, [Phase], [InteractionRule],
           termination_predicates, turn_program).

Rules are (predicate, effect) pairs. Phases form a directed graph that
defines the turn structure. State can include bags, decks, sub-boards,
and overlay networks. Predicates can match spatial patterns of N pieces.
Effects can be stochastic (draw from bag/deck).
"""

from __future__ import annotations
import math
from dataclasses import dataclass, field
from typing import Any

# ═══════════════════════════════════════════════════════════════════════════════
# PREDICATE OPCODES
# ═══════════════════════════════════════════════════════════════════════════════

# Which node: source or target
SRC = 0
TGT = 1

# Owner references
SELF  = 0
ENEMY = 1
ANY_OWNER = 2

# ── Basic predicates ──────────────────────────────────────────────────────────
P_TRUE         = 0
P_EMPTY        = 1    # (which,)
P_OCCUPIED     = 2    # (which,)
P_OWNER_IS     = 3    # (which, owner_ref)
P_TYPE_IS      = 4    # (which, type_id)
P_ADJACENT     = 5    # ()
P_DISTANCE_LEQ = 6    # (max_dist,)
P_RESOURCE_GEQ = 7    # (which, slot, threshold)
P_GLOBAL_GEQ   = 8    # (counter_idx, threshold)
P_TYPE_BEATS   = 9    # ()
P_SAME_TYPE    = 10
P_DIFF_TYPE    = 11

# ── Group predicates ──────────────────────────────────────────────────────────
P_GROUP_HAS_LIBERTY  = 12  # (which,)
P_GROUP_NO_LIBERTY   = 13  # (which,)
P_GROUP_SIZE_GEQ     = 14  # (which, threshold)
P_GROUP_TYPES_GEQ    = 15  # (which, threshold)

# ── N-ary spatial pattern predicates ──────────────────────────────────────────
P_PATTERN_COUNT_GEQ  = 16  # (which, owner_ref, count) → ≥count matching neighbors
P_PATTERN_RING       = 17  # (which, owner_ref, count, spacing) → count pieces at equal angular spacing
P_PATTERN_LINE       = 18  # (which, owner_ref, count) → count collinear pieces through node
P_SAME_REGION        = 19  # () → src and tgt are in same connected same-type region

# ── Stochastic / information predicates ───────────────────────────────────────
P_BAG_NONEMPTY   = 40  # (bag_id,) → bag has items left
P_DECK_NONEMPTY  = 41  # (deck_id,) → deck has cards
P_HAND_HAS       = 42  # (slot, type_id) → hand slot contains type
P_SUBBOARD_AT    = 43  # (subboard_id, position, owner_ref) → check sub-board cell

# ── Combinators ───────────────────────────────────────────────────────────────
P_AND = 20
P_OR  = 21
P_NOT = 22

NUM_PRED_OPS = 50

# ── Termination predicates ────────────────────────────────────────────────────
T_COUNTER_GEQ       = 30
T_NO_PIECES         = 31  # (player_ref,)
T_PIECE_COUNT_GEQ   = 32  # (player_ref, threshold)
T_ROUND_GEQ         = 33  # (threshold,)
T_ONE_PLAYER_LEFT   = 34
T_TERRITORY_GEQ     = 35  # (player_ref, threshold)
T_SCORE_TERRITORY   = 36
T_BAG_EMPTY         = 37  # (bag_id,) → bag is exhausted
T_SUBBOARD_MATCH    = 38  # (subboard_id,) → sub-board pattern matches main board

# ── Winner determination ──────────────────────────────────────────────────────
W_TRIGGERING_PLAYER = 0
W_HIGHEST_COUNTER   = 1
W_MOST_PIECES       = 2
W_LAST_STANDING     = 3
W_TERRITORY         = 4
W_SUBBOARD_SCORE    = 5   # score by matching sub-board pattern

# ═══════════════════════════════════════════════════════════════════════════════
# EFFECT OPCODES
# ═══════════════════════════════════════════════════════════════════════════════

# ── Basic effects ─────────────────────────────────────────────────────────────
E_NOP              = 0
E_REMOVE_PIECE     = 1    # (which,)
E_PLACE_PIECE      = 2    # (which, owner_ref, type_ref)
E_MOVE_PIECE       = 3    # (from_which, to_which)
E_SWAP_PIECES      = 4    # ()
E_PUSH_PIECE       = 5    # (which, away_from)
E_ADD_RESOURCE     = 6    # (which, slot, delta)
E_TRANSFER_RES     = 7    # (from_which, to_which, slot, amount)
E_COLLECT_RES      = 8    # (which, slot)
E_INC_COUNTER      = 9    # (counter_idx, delta)  idx=-1 means current player's counter
E_SET_RESOURCE     = 10   # (which, slot, value)
E_REMOVE_GROUP     = 11   # (which,)
E_INC_COUNTER_BY_GROUP = 12  # (which, counter_idx)

# ── Stochastic effects ────────────────────────────────────────────────────────
E_DRAW_BAG         = 13   # (bag_id, into_slot) → draw random item from bag into resource slot
E_DRAW_DECK        = 14   # (deck_id, into_hand_slot) → draw top card into hand
E_SHUFFLE_DECK     = 15   # (deck_id,) → shuffle discard back into deck
E_DISCARD          = 16   # (deck_id, from_hand_slot) → discard from hand

# ── Sub-board effects ─────────────────────────────────────────────────────────
E_SUBBOARD_PLACE   = 17   # (subboard_id, position, owner_ref) → place token on sub-board
E_SUBBOARD_CLEAR   = 18   # (subboard_id, position) → clear sub-board cell

# ── Network/overlay effects ───────────────────────────────────────────────────
E_LINK_NODES       = 19   # (node1_which, node2_which) → create overlay edge (gate)

# ── Sequencing ────────────────────────────────────────────────────────────────
E_SEQ = 20  # (first, second)

# ── Conditional effect ────────────────────────────────────────────────────────
E_IF  = 21  # (predicate, then_effect, else_effect)

# ── Phase transition ──────────────────────────────────────────────────────────
E_GOTO_PHASE = 22  # (phase_id,) → transition to a different phase in the turn program

NUM_EFFECT_OPS = 30

# Type references for E_PLACE_PIECE
COPY_SRC_TYPE = -10
COPY_TGT_TYPE = -11

# ═══════════════════════════════════════════════════════════════════════════════
# DATA STRUCTURES
# ═══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class BagSpec:
    """A draw bag (like Go's random stones or Journey's world bag)."""
    bag_id:     int = 0
    num_types:  int = 6     # how many distinct item types
    count_each: int = 13    # starting count per type

    def description_bits(self) -> float:
        return math.log2(4) + math.log2(8) + math.log2(16)


@dataclass(frozen=True)
class DeckSpec:
    """A card deck with suits and values."""
    deck_id:    int = 0
    num_suits:  int = 5
    num_values: int = 13

    def description_bits(self) -> float:
        return math.log2(4) + math.log2(8) + math.log2(16)


@dataclass(frozen=True)
class SubBoardSpec:
    """A secondary board structure (like Journey's cipher)."""
    subboard_id: int = 0
    size:        int = 7    # number of cells
    topology:    str = "hex" # how cells connect

    def description_bits(self) -> float:
        return math.log2(4) + math.log2(10) + math.log2(3)


@dataclass(frozen=True)
class StateSchema:
    """Shape of the game state. Everything derives from this."""
    # Topology
    topology_type:     str = "hex"
    topology_size:     int = 4
    topology_symmetry: int = 4
    topology_boundary: str = "finite"
    topology_modifier: str = "none"
    topology_mod_param: int = 0
    # Pieces
    num_piece_types:   int = 2
    num_players:       int = 2
    num_resource_slots: int = 0
    num_global_counters: int = 2
    # Extended state structures
    bags:       tuple = ()   # tuple of BagSpec
    decks:      tuple = ()   # tuple of DeckSpec
    sub_boards: tuple = ()   # tuple of SubBoardSpec
    has_overlay_network: bool = False  # gates / overlay edges

    @property
    def node_fields(self) -> list[str]:
        fields = ["owner", "piece_type"]
        for i in range(self.num_resource_slots):
            fields.append(f"resource_{i}")
        return fields

    @property
    def global_fields(self) -> list[str]:
        fields = ["round", "current_player", "actions_remaining"]
        for i in range(self.num_global_counters):
            fields.append(f"counter_{i}")
        return fields

    def tiling_spec(self):
        from .tiling import TilingSpec
        return TilingSpec(
            base=self.topology_type, size=self.topology_size,
            symmetry=self.topology_symmetry, boundary=self.topology_boundary,
            modifier=self.topology_modifier, mod_param=self.topology_mod_param,
        )

    def description_bits(self) -> float:
        bits = self.tiling_spec().description_bits()
        bits += math.log2(4)   # num_piece_types
        bits += math.log2(4)   # num_players
        bits += math.log2(3)   # num_resource_slots
        bits += math.log2(4)   # num_global_counters
        for bag in self.bags:
            bits += bag.description_bits()
        for deck in self.decks:
            bits += deck.description_bits()
        for sb in self.sub_boards:
            bits += sb.description_bits()
        if self.has_overlay_network:
            bits += 1.0
        return bits


# ── Turn program: phases as a directed graph ──────────────────────────────────

@dataclass(frozen=True)
class Phase:
    """A node in the turn program graph.

    Each phase has rules (choices available to the player) and transitions
    (which phase to go to after a choice is made, or automatically).
    """
    phase_id:   int
    name:       str = ""
    rules:      tuple = ()            # tuple of Rule — choices in this phase
    auto_next:  int = -1              # if ≥0, auto-advance to this phase (no player choice)
    on_complete: int = -1             # phase to go to after a rule is executed
    decider:    str = "current"       # "current" | "owner" | "captain" | player index
    # For phases with no rules and auto_next, this is a "pass-through" phase
    # that can apply an automatic effect
    auto_effect: tuple = (E_NOP,)

    def description_bits(self) -> float:
        bits = 3.0  # phase_id encoding
        bits += _ast_bits(self.auto_effect)
        for rule in self.rules:
            bits += rule.description_bits()
        bits += 3.0  # transitions
        if self.decider != "current":
            bits += math.log2(4)
        return bits


@dataclass(frozen=True)
class TurnProgram:
    """The turn structure as a phase graph.

    Simple games: one phase (phase 0) with all rules, on_complete = advance turn.
    Complex games: multiple phases with transitions between them.

    Phase -1 means "advance to next player's turn".
    """
    phases: tuple = ()  # tuple of Phase

    def description_bits(self) -> float:
        return sum(p.description_bits() for p in self.phases)


# ── Rules and interactions ────────────────────────────────────────────────────

@dataclass(frozen=True)
class Rule:
    """A player-chosen action: when predicate holds, effect is legal."""
    predicate:     tuple
    effect:        tuple
    requires_piece: bool = True

    def description_bits(self) -> float:
        return _ast_bits(self.predicate) + _ast_bits(self.effect) + 1.0


@dataclass(frozen=True)
class InteractionRule:
    """Automatic trigger."""
    trigger:   str = "adjacency"
    predicate: tuple = (P_TRUE,)
    effect:    tuple = (E_NOP,)

    def description_bits(self) -> float:
        return math.log2(3) + _ast_bits(self.predicate) + _ast_bits(self.effect)


@dataclass(frozen=True)
class TerminationSpec:
    predicate:   tuple
    winner_mode: int = W_TRIGGERING_PLAYER
    winner_arg:  int = 0

    def description_bits(self) -> float:
        return _ast_bits(self.predicate) + math.log2(6) + 2.0


# ═══════════════════════════════════════════════════════════════════════════════
# RULESET V3 (extended)
# ═══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class RuleSetV3:
    """Universal game definition.

    For simple games: use `rules` (flat list, one phase).
    For complex games: use `turn_program` (phase graph).
    If turn_program is non-empty, it takes precedence over rules.
    """
    schema:         StateSchema
    conflict_table: tuple = ()
    # Simple mode: flat rule list (backward compatible)
    rules:          tuple = ()
    interactions:   tuple = ()
    terminations:   tuple = ()
    actions_per_turn: int = 1
    turn_limit:     int = 50
    placement:      str = "spread"
    # Complex mode: phase graph
    turn_program:   TurnProgram | None = None

    @property
    def has_turn_program(self) -> bool:
        return self.turn_program is not None and len(self.turn_program.phases) > 0

    @property
    def all_rules(self) -> tuple:
        """All rules from either flat list or phase graph."""
        if self.has_turn_program:
            result = []
            for phase in self.turn_program.phases:
                result.extend(phase.rules)
            return tuple(result)
        return self.rules

    def description_length(self) -> float:
        bits = self.schema.description_bits()
        T = self.schema.num_piece_types
        n_pairs = T * (T - 1) // 2
        bits += n_pairs * math.log2(4)
        if self.has_turn_program:
            bits += self.turn_program.description_bits()
        else:
            for rule in self.rules:
                bits += rule.description_bits()
        for ir in self.interactions:
            bits += ir.description_bits()
        for ts in self.terminations:
            bits += ts.description_bits()
        bits += math.log2(3)  # actions_per_turn
        bits += math.log2(4)  # turn_limit
        bits += math.log2(4)  # placement
        return bits

    def short_id(self) -> str:
        s = self.schema
        nr = len(self.all_rules)
        return f"{s.topology_type[:3]}{s.topology_size}t{s.num_piece_types}p{s.num_players}r{nr}"

    def to_dict(self) -> dict:
        d = {
            "schema": {
                "topology_type": self.schema.topology_type,
                "topology_size": self.schema.topology_size,
                "topology_symmetry": self.schema.topology_symmetry,
                "topology_boundary": self.schema.topology_boundary,
                "topology_modifier": self.schema.topology_modifier,
                "topology_mod_param": self.schema.topology_mod_param,
                "num_piece_types": self.schema.num_piece_types,
                "num_players": self.schema.num_players,
                "num_resource_slots": self.schema.num_resource_slots,
                "num_global_counters": self.schema.num_global_counters,
                "bags": [{"bag_id": b.bag_id, "num_types": b.num_types,
                          "count_each": b.count_each} for b in self.schema.bags],
                "decks": [{"deck_id": d.deck_id, "num_suits": d.num_suits,
                           "num_values": d.num_values} for d in self.schema.decks],
                "sub_boards": [{"subboard_id": sb.subboard_id, "size": sb.size,
                                "topology": sb.topology} for sb in self.schema.sub_boards],
                "has_overlay_network": self.schema.has_overlay_network,
            },
            "conflict_table": list(self.conflict_table),
            "rules": [{"predicate": r.predicate, "effect": r.effect,
                        "requires_piece": r.requires_piece} for r in self.rules],
            "interactions": [{"trigger": ir.trigger, "predicate": ir.predicate,
                              "effect": ir.effect} for ir in self.interactions],
            "terminations": [{"predicate": ts.predicate, "winner_mode": ts.winner_mode,
                              "winner_arg": ts.winner_arg} for ts in self.terminations],
            "actions_per_turn": self.actions_per_turn,
            "turn_limit": self.turn_limit,
            "placement": self.placement,
        }
        if self.has_turn_program:
            d["turn_program"] = {
                "phases": [{"phase_id": p.phase_id, "name": p.name,
                            "rules": [{"predicate": r.predicate, "effect": r.effect,
                                        "requires_piece": r.requires_piece} for r in p.rules],
                            "auto_next": p.auto_next, "on_complete": p.on_complete,
                            "decider": p.decider, "auto_effect": p.auto_effect}
                           for p in self.turn_program.phases]
            }
        return d

    @staticmethod
    def from_dict(d: dict) -> "RuleSetV3":
        s = d.get("schema", {})
        bags = tuple(BagSpec(**b) for b in s.get("bags", []))
        decks = tuple(DeckSpec(**dk) for dk in s.get("decks", []))
        sub_boards = tuple(SubBoardSpec(**sb) for sb in s.get("sub_boards", []))
        schema = StateSchema(
            topology_type=s.get("topology_type", "hex"),
            topology_size=s.get("topology_size", 4),
            topology_symmetry=s.get("topology_symmetry", 4),
            topology_boundary=s.get("topology_boundary", "finite"),
            topology_modifier=s.get("topology_modifier", "none"),
            topology_mod_param=s.get("topology_mod_param", 0),
            num_piece_types=s.get("num_piece_types", 2),
            num_players=s.get("num_players", 2),
            num_resource_slots=s.get("num_resource_slots", 0),
            num_global_counters=s.get("num_global_counters", 2),
            bags=bags, decks=decks, sub_boards=sub_boards,
            has_overlay_network=s.get("has_overlay_network", False),
        )
        tp_data = d.get("turn_program")
        turn_program = None
        if tp_data:
            phases = []
            for p in tp_data.get("phases", []):
                rules = tuple(Rule(
                    predicate=tuple(r["predicate"]) if isinstance(r["predicate"], list) else r["predicate"],
                    effect=tuple(r["effect"]) if isinstance(r["effect"], list) else r["effect"],
                    requires_piece=r.get("requires_piece", True),
                ) for r in p.get("rules", []))
                phases.append(Phase(
                    phase_id=p["phase_id"], name=p.get("name", ""),
                    rules=rules, auto_next=p.get("auto_next", -1),
                    on_complete=p.get("on_complete", -1),
                    decider=p.get("decider", "current"),
                    auto_effect=tuple(p.get("auto_effect", (E_NOP,))),
                ))
            turn_program = TurnProgram(phases=tuple(phases))
        return RuleSetV3(
            schema=schema,
            conflict_table=tuple(d.get("conflict_table", ())),
            rules=tuple(Rule(
                predicate=tuple(r["predicate"]) if isinstance(r["predicate"], list) else r["predicate"],
                effect=tuple(r["effect"]) if isinstance(r["effect"], list) else r["effect"],
                requires_piece=r.get("requires_piece", True),
            ) for r in d.get("rules", ())),
            interactions=tuple(InteractionRule(
                trigger=ir.get("trigger", "adjacency"),
                predicate=tuple(ir["predicate"]) if isinstance(ir["predicate"], list) else ir["predicate"],
                effect=tuple(ir["effect"]) if isinstance(ir["effect"], list) else ir["effect"],
            ) for ir in d.get("interactions", ())),
            terminations=tuple(TerminationSpec(
                predicate=tuple(ts["predicate"]) if isinstance(ts["predicate"], list) else ts["predicate"],
                winner_mode=ts.get("winner_mode", W_TRIGGERING_PLAYER),
                winner_arg=ts.get("winner_arg", 0),
            ) for ts in d.get("terminations", ())),
            actions_per_turn=d.get("actions_per_turn", 1),
            turn_limit=d.get("turn_limit", 50),
            placement=d.get("placement", "spread"),
            turn_program=turn_program,
        )


# ═══════════════════════════════════════════════════════════════════════════════
# AST UTILITIES
# ═══════════════════════════════════════════════════════════════════════════════

def _ast_bits(node) -> float:
    if not isinstance(node, tuple) or len(node) == 0:
        return 0.0
    bits = math.log2(max(NUM_PRED_OPS, NUM_EFFECT_OPS))
    for arg in node[1:]:
        if isinstance(arg, tuple):
            bits += _ast_bits(arg)
        elif isinstance(arg, (int, float)):
            bits += 3.0
        elif isinstance(arg, bool):
            bits += 1.0
    return bits


def fields_referenced(ast_node) -> set[str]:
    """Walk AST and collect state field references."""
    fields: set[str] = set()
    if not isinstance(ast_node, tuple) or len(ast_node) == 0:
        return fields
    op = ast_node[0]
    if op in (P_EMPTY, P_OCCUPIED):
        fields.add("owner")
    elif op == P_OWNER_IS:
        fields.add("owner")
    elif op in (P_TYPE_IS, P_TYPE_BEATS, P_SAME_TYPE, P_DIFF_TYPE):
        fields.add("piece_type")
    elif op == P_RESOURCE_GEQ:
        fields.add(f"resource_{ast_node[2]}")
    elif op in (P_GLOBAL_GEQ, T_COUNTER_GEQ):
        idx = ast_node[1] if len(ast_node) > 1 else 0
        if idx < 0:
            fields.update(f"counter_{i}" for i in range(10))
        else:
            fields.add(f"counter_{idx}")
    elif op in (T_NO_PIECES, T_PIECE_COUNT_GEQ, T_ONE_PLAYER_LEFT):
        fields.add("owner")
    elif op in (P_GROUP_HAS_LIBERTY, P_GROUP_NO_LIBERTY, P_GROUP_SIZE_GEQ, P_GROUP_TYPES_GEQ):
        fields.update(["owner", "piece_type"])
    elif op in (P_PATTERN_COUNT_GEQ, P_PATTERN_RING, P_PATTERN_LINE, P_SAME_REGION):
        fields.update(["owner", "piece_type"])
    elif op in (P_BAG_NONEMPTY, T_BAG_EMPTY):
        fields.add("bag")
    elif op in (P_DECK_NONEMPTY,):
        fields.add("deck")
    elif op in (P_SUBBOARD_AT, T_SUBBOARD_MATCH):
        fields.add("sub_board")
    elif op in (E_REMOVE_PIECE, E_PLACE_PIECE, E_MOVE_PIECE, E_SWAP_PIECES,
                E_PUSH_PIECE, E_REMOVE_GROUP):
        fields.update(["owner", "piece_type"])
    elif op in (E_ADD_RESOURCE, E_SET_RESOURCE, E_COLLECT_RES):
        fields.add(f"resource_{ast_node[2] if len(ast_node) > 2 else 0}")
    elif op == E_TRANSFER_RES:
        fields.add(f"resource_{ast_node[3] if len(ast_node) > 3 else 0}")
    elif op == E_INC_COUNTER:
        idx = ast_node[1] if len(ast_node) > 1 else 0
        if idx < 0:
            fields.update(f"counter_{i}" for i in range(10))
        else:
            fields.add(f"counter_{idx}")
    elif op == E_INC_COUNTER_BY_GROUP:
        fields.update(["owner", "piece_type"])
        idx = ast_node[2] if len(ast_node) > 2 else 0
        if idx < 0:
            fields.update(f"counter_{i}" for i in range(10))
        else:
            fields.add(f"counter_{idx}")
    elif op in (E_DRAW_BAG,):
        fields.add("bag")
    elif op in (E_DRAW_DECK, E_SHUFFLE_DECK, E_DISCARD):
        fields.add("deck")
    elif op in (E_SUBBOARD_PLACE, E_SUBBOARD_CLEAR):
        fields.add("sub_board")
    elif op == E_LINK_NODES:
        fields.add("overlay_network")
    # Recurse
    for arg in ast_node[1:]:
        if isinstance(arg, tuple):
            fields |= fields_referenced(arg)
    return fields


def validate_coverage(rs: RuleSetV3) -> bool:
    """Every non-implicit state field must be referenced by at least one rule."""
    all_refs: set[str] = set()
    for rule in rs.all_rules:
        all_refs |= fields_referenced(rule.predicate)
        all_refs |= fields_referenced(rule.effect)
    for ir in rs.interactions:
        all_refs |= fields_referenced(ir.predicate)
        all_refs |= fields_referenced(ir.effect)
    for ts in rs.terminations:
        all_refs |= fields_referenced(ts.predicate)
    if rs.has_turn_program:
        for phase in rs.turn_program.phases:
            all_refs |= fields_referenced(phase.auto_effect)

    required = set(rs.schema.node_fields)
    for i in range(rs.schema.num_global_counters):
        required.add(f"counter_{i}")
    return required.issubset(all_refs)
