"""AST interpreter for predicate and effect evaluation.

Evaluates predicate trees to bool and applies effect trees to game state.
No hardcoded game logic — everything is driven by the AST.
"""

from __future__ import annotations
from collections import deque

from .schema import *


def eval_predicate(pred: tuple, state: dict, schema: StateSchema,
                   topo, src: int, tgt: int, player: int,
                   conflict_table: tuple) -> bool:
    """Evaluate a predicate AST to bool."""
    if not isinstance(pred, tuple) or len(pred) == 0:
        return True
    op = pred[0]

    if op == P_TRUE:
        return True

    elif op == P_EMPTY:
        node = src if pred[1] == SRC else tgt
        nd = state["nodes"].get(node)
        return nd is None or nd.get("owner", -1) < 0

    elif op == P_OCCUPIED:
        node = src if pred[1] == SRC else tgt
        nd = state["nodes"].get(node)
        return nd is not None and nd.get("owner", -1) >= 0

    elif op == P_OWNER_IS:
        node = src if pred[1] == SRC else tgt
        nd = state["nodes"].get(node)
        if nd is None:
            return False
        owner = nd.get("owner", -1)
        ref = pred[2]
        if ref == SELF:
            return owner == player
        elif ref == ENEMY:
            return owner >= 0 and owner != player
        elif ref == ANY_OWNER:
            return owner >= 0
        return owner == ref

    elif op == P_TYPE_IS:
        node = src if pred[1] == SRC else tgt
        nd = state["nodes"].get(node)
        if nd is None:
            return False
        return nd.get("piece_type", -1) == pred[2]

    elif op == P_ADJACENT:
        return tgt in topo.adjacencies.get(src, [])

    elif op == P_DISTANCE_LEQ:
        return _bfs_distance(topo, src, tgt) <= pred[1]

    elif op == P_RESOURCE_GEQ:
        node = src if pred[1] == SRC else tgt
        nd = state["nodes"].get(node)
        if nd is None:
            return False
        slot = f"resource_{pred[2]}"
        return nd.get(slot, 0) >= pred[3]

    elif op == P_GLOBAL_GEQ:
        return state["globals"].get(f"counter_{pred[1]}", 0) >= pred[2]

    elif op == P_TYPE_BEATS:
        src_nd = state["nodes"].get(src)
        tgt_nd = state["nodes"].get(tgt)
        if not src_nd or not tgt_nd:
            return False
        st = src_nd.get("piece_type", -1)
        tt = tgt_nd.get("piece_type", -1)
        if st < 0 or tt < 0 or st == tt:
            return False
        return _conflict_outcome(conflict_table, schema.num_piece_types, st, tt) == "wins"

    elif op == P_SAME_TYPE:
        src_nd = state["nodes"].get(src)
        tgt_nd = state["nodes"].get(tgt)
        if not src_nd or not tgt_nd:
            return False
        return src_nd.get("piece_type", -1) == tgt_nd.get("piece_type", -1)

    elif op == P_DIFF_TYPE:
        src_nd = state["nodes"].get(src)
        tgt_nd = state["nodes"].get(tgt)
        if not src_nd or not tgt_nd:
            return False
        st, tt = src_nd.get("piece_type", -1), tgt_nd.get("piece_type", -1)
        return st >= 0 and tt >= 0 and st != tt

    elif op == P_GROUP_HAS_LIBERTY:
        node = src if pred[1] == SRC else tgt
        group = _find_group(state, topo, node)
        return _group_liberties(state, topo, group) > 0 if group else False

    elif op == P_GROUP_NO_LIBERTY:
        node = src if pred[1] == SRC else tgt
        group = _find_group(state, topo, node)
        return _group_liberties(state, topo, group) == 0 if group else False

    elif op == P_GROUP_SIZE_GEQ:
        node = src if pred[1] == SRC else tgt
        threshold = pred[2] if len(pred) > 2 else 1
        group = _find_group(state, topo, node)
        return len(group) >= threshold

    elif op == P_GROUP_TYPES_GEQ:
        node = src if pred[1] == SRC else tgt
        threshold = pred[2] if len(pred) > 2 else 2
        group = _find_group(state, topo, node)
        return len(_group_types(state, group)) >= threshold

    elif op == P_AND:
        return (eval_predicate(pred[1], state, schema, topo, src, tgt, player, conflict_table) and
                eval_predicate(pred[2], state, schema, topo, src, tgt, player, conflict_table))

    elif op == P_OR:
        return (eval_predicate(pred[1], state, schema, topo, src, tgt, player, conflict_table) or
                eval_predicate(pred[2], state, schema, topo, src, tgt, player, conflict_table))

    elif op == P_NOT:
        return not eval_predicate(pred[1], state, schema, topo, src, tgt, player, conflict_table)

    return True


def apply_effect(eff: tuple, state: dict, schema: StateSchema,
                 topo, src: int, tgt: int, player: int) -> dict:
    """Apply an effect AST to game state (mutates in place — caller deep-copies)."""
    if not isinstance(eff, tuple) or len(eff) == 0:
        return state
    op = eff[0]

    if op == E_NOP:
        pass

    elif op == E_REMOVE_PIECE:
        node = src if eff[1] == SRC else tgt
        if node in state["nodes"] and state["nodes"][node].get("owner", -1) >= 0:
            state["nodes"][node] = _empty_node(schema)

    elif op == E_PLACE_PIECE:
        node = src if eff[1] == SRC else tgt
        owner_ref = eff[2] if len(eff) > 2 else SELF
        type_ref = eff[3] if len(eff) > 3 else 0
        owner = player if owner_ref == SELF else owner_ref
        ptype = _resolve_type(type_ref, state, src, tgt, schema)
        nd = _empty_node(schema)
        nd["owner"] = owner
        nd["piece_type"] = ptype
        state["nodes"][node] = nd

    elif op == E_MOVE_PIECE:
        from_n = src if eff[1] == SRC else tgt
        to_n = tgt if eff[2] == TGT else src
        if from_n in state["nodes"] and state["nodes"][from_n].get("owner", -1) >= 0:
            state["nodes"][to_n] = dict(state["nodes"][from_n])
            state["nodes"][from_n] = _empty_node(schema)

    elif op == E_SWAP_PIECES:
        nd_src = dict(state["nodes"].get(src, _empty_node(schema)))
        nd_tgt = dict(state["nodes"].get(tgt, _empty_node(schema)))
        state["nodes"][src] = nd_tgt
        state["nodes"][tgt] = nd_src

    elif op == E_PUSH_PIECE:
        which_n = src if eff[1] == SRC else tgt
        away_n = src if eff[2] == SRC else tgt
        nd = state["nodes"].get(which_n)
        if nd and nd.get("owner", -1) >= 0:
            # Find empty neighbor away from push source
            candidates = [n for n in topo.adjacencies.get(which_n, [])
                          if n != away_n and
                          state["nodes"].get(n, {}).get("owner", -1) < 0]
            if candidates:
                dest = candidates[0]
                state["nodes"][dest] = dict(nd)
                state["nodes"][which_n] = _empty_node(schema)

    elif op == E_ADD_RESOURCE:
        node = src if eff[1] == SRC else tgt
        slot = f"resource_{eff[2]}" if len(eff) > 2 else "resource_0"
        delta = eff[3] if len(eff) > 3 else 1
        nd = state["nodes"].get(node)
        if nd:
            nd[slot] = max(0, nd.get(slot, 0) + delta)

    elif op == E_TRANSFER_RES:
        from_n = src if eff[1] == SRC else tgt
        to_n = tgt if eff[2] == TGT else src
        slot = f"resource_{eff[3]}" if len(eff) > 3 else "resource_0"
        amount = eff[4] if len(eff) > 4 else 1
        from_nd = state["nodes"].get(from_n)
        to_nd = state["nodes"].get(to_n)
        if from_nd and to_nd:
            actual = min(amount, from_nd.get(slot, 0))
            from_nd[slot] = from_nd.get(slot, 0) - actual
            to_nd[slot] = to_nd.get(slot, 0) + actual

    elif op == E_COLLECT_RES:
        node = src if eff[1] == SRC else tgt
        slot = f"resource_{eff[2]}" if len(eff) > 2 else "resource_0"
        nd = state["nodes"].get(node)
        if nd:
            # Collect from node's free resource
            free = nd.get(slot, 0) if nd.get("owner", -1) < 0 else 0
            if free > 0 and src in state["nodes"]:
                state["nodes"][src][slot] = state["nodes"][src].get(slot, 0) + free
                nd[slot] = 0

    elif op == E_INC_COUNTER:
        idx = eff[1] if len(eff) > 1 else 0
        delta = eff[2] if len(eff) > 2 else 1
        # idx == -1 means "my counter" (counter_{player_id})
        actual_idx = player if idx < 0 else idx
        key = f"counter_{actual_idx}"
        state["globals"][key] = state["globals"].get(key, 0) + delta

    elif op == E_SET_RESOURCE:
        node = src if eff[1] == SRC else tgt
        slot = f"resource_{eff[2]}" if len(eff) > 2 else "resource_0"
        value = eff[3] if len(eff) > 3 else 0
        nd = state["nodes"].get(node)
        if nd:
            nd[slot] = value

    elif op == E_REMOVE_GROUP:
        node = src if eff[1] == SRC else tgt
        group = _find_group(state, topo, node)
        for n in group:
            state["nodes"][n] = _empty_node(schema)

    elif op == E_INC_COUNTER_BY_GROUP:
        node = src if eff[1] == SRC else tgt
        counter_idx = eff[2] if len(eff) > 2 else 0
        actual_idx = player if counter_idx < 0 else counter_idx
        group = _find_group(state, topo, node)
        key = f"counter_{actual_idx}"
        state["globals"][key] = state["globals"].get(key, 0) + len(group)

    elif op == E_SEQ:
        state = apply_effect(eff[1], state, schema, topo, src, tgt, player)
        state = apply_effect(eff[2], state, schema, topo, src, tgt, player)

    return state


# ── Termination evaluation ────────────────────────────────────────────────────

def eval_termination(pred: tuple, state: dict, schema: StateSchema,
                     player: int, topo=None) -> bool:
    """Evaluate a termination predicate."""
    if not isinstance(pred, tuple) or len(pred) == 0:
        return False
    op = pred[0]

    if op == T_COUNTER_GEQ:
        idx = pred[1] if len(pred) > 1 else 0
        threshold = pred[2] if len(pred) > 2 else 1
        return state["globals"].get(f"counter_{idx}", 0) >= threshold

    elif op == T_NO_PIECES:
        ref = pred[1] if len(pred) > 1 else ENEMY
        if ref == SELF:
            check_player = player
        elif ref == ENEMY:
            # Any enemy has no pieces
            for p in range(schema.num_players):
                if p != player and not any(
                    nd.get("owner") == p for nd in state["nodes"].values()
                ):
                    return True
            return False
        else:
            check_player = ref
        return not any(nd.get("owner") == check_player for nd in state["nodes"].values())

    elif op == T_PIECE_COUNT_GEQ:
        ref = pred[1] if len(pred) > 1 else SELF
        threshold = pred[2] if len(pred) > 2 else 5
        check_player = player if ref == SELF else ref
        count = sum(1 for nd in state["nodes"].values() if nd.get("owner") == check_player)
        return count >= threshold

    elif op == T_ROUND_GEQ:
        threshold = pred[1] if len(pred) > 1 else 50
        return state["globals"].get("round", 0) >= threshold

    elif op == T_ONE_PLAYER_LEFT:
        alive = set()
        for nd in state["nodes"].values():
            owner = nd.get("owner", -1)
            if owner >= 0:
                alive.add(owner)
        return len(alive) <= 1

    elif op == T_SCORE_TERRITORY:
        # Compute Go-style territory: flood-fill empty regions, assign to
        # the enclosing player (if exactly one player borders the region).
        # Add territory to each player's counter.
        visited = set()
        for node_id, nd in state["nodes"].items():
            if nd.get("owner", -1) >= 0 or node_id in visited:
                continue
            # BFS to find connected empty region
            region = set()
            bordering_players = set()
            frontier = deque([node_id])
            while frontier:
                n = frontier.popleft()
                if n in region:
                    continue
                n_nd = state["nodes"].get(n)
                if n_nd and n_nd.get("owner", -1) >= 0:
                    bordering_players.add(n_nd["owner"])
                    continue
                region.add(n)
                visited.add(n)
                for nb in topo.adjacencies.get(n, []) if topo else []:
                    if nb not in region:
                        frontier.append(nb)
            # If exactly one player borders this region, it's their territory
            if len(bordering_players) == 1:
                p = next(iter(bordering_players))
                key = f"counter_{p}"
                state["globals"][key] = state["globals"].get(key, 0) + len(region)
        return True  # always "fires" — it's a scoring action, not a condition

    elif op == T_TERRITORY_GEQ:
        ref = pred[1] if len(pred) > 1 else SELF
        threshold = pred[2] if len(pred) > 2 else 50
        check_player = player if ref == SELF else ref
        total = len(state["nodes"])
        controlled = sum(1 for nd in state["nodes"].values() if nd.get("owner") == check_player)
        return total > 0 and (controlled * 100) >= (threshold * total)

    return False


# ── Helpers ───────────────────────────────────────────────────────────────────

def _find_group(state: dict, topo, node: int) -> set[int]:
    """Find the connected group of same-owner pieces containing node."""
    nd = state["nodes"].get(node)
    if not nd or nd.get("owner", -1) < 0:
        return set()
    owner = nd["owner"]
    visited = set()
    frontier = deque([node])
    while frontier:
        n = frontier.popleft()
        if n in visited:
            continue
        visited.add(n)
        for nb in topo.adjacencies.get(n, []):
            if nb not in visited:
                nb_nd = state["nodes"].get(nb)
                if nb_nd and nb_nd.get("owner") == owner:
                    frontier.append(nb)
    return visited


def _group_liberties(state: dict, topo, group: set[int]) -> int:
    """Count empty nodes adjacent to a group."""
    liberties = set()
    for n in group:
        for nb in topo.adjacencies.get(n, []):
            if nb not in group:
                nb_nd = state["nodes"].get(nb)
                if not nb_nd or nb_nd.get("owner", -1) < 0:
                    liberties.add(nb)
    return len(liberties)


def _group_types(state: dict, group: set[int]) -> set[int]:
    """Distinct piece types in a group."""
    types = set()
    for n in group:
        nd = state["nodes"].get(n)
        if nd:
            t = nd.get("piece_type", -1)
            if t >= 0:
                types.add(t)
    return types


def predicate_max_range(pred: tuple) -> int:
    """Extract the maximum distance constraint from a predicate for search optimization."""
    if not isinstance(pred, tuple) or len(pred) == 0:
        return 99
    op = pred[0]
    if op == P_ADJACENT:
        return 1
    if op == P_DISTANCE_LEQ:
        return pred[1]
    if op == P_AND:
        return min(predicate_max_range(pred[1]), predicate_max_range(pred[2]))
    if op == P_OR:
        return max(predicate_max_range(pred[1]), predicate_max_range(pred[2]))
    if op == P_NOT:
        return 99  # NOT doesn't constrain distance
    return 99


def _empty_node(schema: StateSchema) -> dict:
    """Create an empty node dict."""
    nd = {"owner": -1, "piece_type": -1}
    for i in range(schema.num_resource_slots):
        nd[f"resource_{i}"] = 0
    return nd


def _resolve_type(type_ref: int, state: dict, src: int, tgt: int,
                  schema: StateSchema) -> int:
    """Resolve a type reference to a concrete piece type."""
    if type_ref == COPY_SRC_TYPE:
        nd = state["nodes"].get(src)
        return nd.get("piece_type", 0) if nd else 0
    if type_ref == COPY_TGT_TYPE:
        nd = state["nodes"].get(tgt)
        return nd.get("piece_type", 0) if nd else 0
    if type_ref < 0:
        return 0
    return type_ref % max(schema.num_piece_types, 1)


def _bfs_distance(topo, a: int, b: int) -> int:
    """Shortest path distance between two nodes."""
    if a == b:
        return 0
    visited = {a}
    frontier = deque([(a, 0)])
    while frontier:
        node, dist = frontier.popleft()
        for nb in topo.adjacencies.get(node, []):
            if nb == b:
                return dist + 1
            if nb not in visited:
                visited.add(nb)
                frontier.append((nb, dist + 1))
    return 999


def _conflict_index(num_types: int, i: int, j: int) -> int:
    a, b = min(i, j), max(i, j)
    idx = 0
    for x in range(a):
        idx += num_types - 1 - x
    return idx + b - a - 1


def _conflict_outcome(conflict_table: tuple, num_types: int,
                      attacker: int, defender: int) -> str:
    """Returns 'wins', 'loses', 'coexist', or 'mutual'."""
    if attacker == defender:
        return "coexist"
    idx = _conflict_index(num_types, attacker, defender)
    if idx >= len(conflict_table):
        return "coexist"
    raw = conflict_table[idx]
    if raw == 0:
        return "coexist"
    if raw == 3:
        return "mutual"
    i = min(attacker, defender)
    winner = i if raw == 1 else max(attacker, defender)
    return "wins" if winner == attacker else "loses"
