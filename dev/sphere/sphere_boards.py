"""Choose and export the SPHERE mutation boards.

One core (2-4 players): the core is a cell on a k-fold axis, homes are one
orbit of a home triangle under rotation about it.
Two cores (5-6 players): cores at both ends of an axis; homes are one orbit
under C5 (5p, pentagon axis) or D3 (6p, three-fold axis plus the half-turns
that swap the cores), so every player sees the same board.
"""
import json, sys
import numpy as np
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import shortest_path
from sphere import gp, icosahedron, octahedron, rotation, axes

def setup(kind, m, n):
    pts, adj, tris, v, faces = gp(icosahedron() if kind == 'ico' else octahedron(), m, n)
    N = len(pts)
    rows = [a for a in range(N) for _ in adj[a]]
    cols = [b for a in range(N) for b in adj[a]]
    D = shortest_path(csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(N, N)), unweighted=True)
    return pts, adj, tris, v, faces, D

def perm_of(pts, R):
    img = pts @ R.T
    p = np.argmax(img @ pts.T, axis=1)
    return p if np.allclose(pts[p], img, atol=1e-6) else None

def cell_on(pts, axis):
    hit = np.where(pts @ axis > 1 - 1e-9)[0]
    return int(hit[0]) if len(hit) else None

def orbit(t, gens):
    seen = {t}
    frontier = [t]
    while frontier:
        nxt = []
        for x in frontier:
            for g in gens:
                y = tuple(sorted(int(c) for c in g[list(x)]))
                if y not in seen:
                    seen.add(y); nxt.append(y)
        frontier = nxt
    return list(seen)

def best_layout(players, kind, m, n, cores_wanted):
    pts, adj, tris, v, faces, D = setup(kind, m, n)
    defects = {i for i in range(len(pts)) if len(adj[i]) != 6}
    best = None
    for order, axis in axes(v, faces):
        if cores_wanted == 1 and order % players:
            continue
        if cores_wanted == 2 and not ((players == 5 and order == 5) or (players == 6 and order == 3)):
            continue
        core = cell_on(pts, axis)
        if core is None:
            continue
        cores = [core]
        rot = perm_of(pts, rotation(axis, 2 * np.pi / (players if cores_wanted == 1 else order)))
        if rot is None:
            continue
        gens = [rot]
        if cores_wanted == 2:
            anti = cell_on(pts, -axis)
            if anti is None:
                continue
            cores.append(anti)
            if players == 6:
                # a half-turn about an axis perpendicular to this one swaps the cores
                flip = None
                for k2, a2 in all_axes(v, faces, 2):
                    if abs(a2 @ axis) < 1e-9:
                        flip = perm_of(pts, rotation(a2, np.pi))
                        if flip is not None:
                            break
                if flip is None:
                    continue
                gens.append(flip)
        for t in tris:
            if set(t) & set(cores):
                continue
            homes = orbit(t, gens)
            if len(homes) != players:
                continue
            cells = [c for h in homes for c in h]
            if len(set(cells)) != 3 * players:
                continue
            h0 = list(t)
            pg = min(D[np.ix_(h0, list(h))].min() for h in homes if h != t) - 1
            cg = [D[h0, c].min() - 1 for c in cores]
            touch = bool(set(cells) & defects)
            score = (min(pg, min(cg)), pg, -abs(cg[0] - cg[-1]), -touch)
            if best is None or score > best[0]:
                best = (score, dict(players=players, cells=len(pts), grid=[kind, m, n], player_gap=int(pg),
                                    core_gaps=[int(x) for x in cg], home_on_defect=touch,
                                    cores=cores, homes=homes, axis=axis.tolist()))
    return best[1] if best else None

def all_axes(v, faces, order):
    out = []
    for a, b, c in faces:
        for x, y in [(a, b), (b, c), (c, a)]:
            ax = (v[x] + v[y]) / 2
            out.append((2, ax / np.linalg.norm(ax)))
    return out

SIZES = sorted({(m, n) for m in range(1, 9) for n in range(0, m + 1)},
               key=lambda mn: mn[0] ** 2 + mn[0] * mn[1] + mn[1] ** 2)

def smallest(players, cores_wanted, kinds, need_pg=3, need_cg=3, max_cells=700):
    found = []
    for kind in kinds:
        for m, n in SIZES:
            T = m * m + m * n + n * n
            if (10 if kind == 'ico' else 4) * T + 2 > max_cells:
                continue
            r = best_layout(players, kind, m, n, cores_wanted)
            if r:
                found.append(r)
    found.sort(key=lambda r: r['cells'])
    return [r for r in found if r['player_gap'] >= need_pg and min(r['core_gaps']) >= need_cg]

if __name__ == '__main__':
    for players, cores_wanted, kinds in [(5, 2, ['ico']), (6, 2, ['ico', 'oct'])]:
        rs = smallest(players, cores_wanted, kinds, need_pg=3, need_cg=2)
        for r in rs[:4]:
            print({k: r[k] for k in ['players', 'cells', 'grid', 'player_gap', 'core_gaps', 'home_on_defect']})
        sys.stdout.flush()
