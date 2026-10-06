"""Write src/cljc/organism/sphere.cljc: the SPHERE mutation's boards as data.

Each board is built, its symmetry checked, its homes chosen, then rotated so
the (first) core is at +y, the 3D view's up. Spaces are named as on the flat
boards, [ring index]: ring "A" is the core(s), each later ring one step
further from the nearest core, indexed around the axis.
"""
import math, sys
import numpy as np
from collections import deque
from sphere import gp, icosahedron, octahedron, rotation, axes
from sphere_boards import perm_of, cell_on, orbit, all_axes
from bipy import bipyramid
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import shortest_path

LETTERS = list("ABCDEFGHIJKLM")

# players: (base, m, n, axis order used, cores)
CHOICES = {2: ('oct', 3, 0, 4, 1), 3: ('oct', 4, 1, 3, 1), 4: ('oct', 3, 2, 4, 1),
           5: ('bipy5', 3, 2, 5, 2), 6: ('oct', 3, 3, 3, 2)}

def base_of(kind):
    return {'ico': icosahedron, 'oct': octahedron}.get(kind, lambda: bipyramid(5, 1.0))()

def build(players):
    kind, m, n, order, ncores = CHOICES[players]
    pts, adj, tris, v, faces = gp(base_of(kind), m, n)
    N = len(pts)
    rows = [a for a in range(N) for _ in adj[a]]; cols = [b for a in range(N) for b in adj[a]]
    D = shortest_path(csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(N, N)), unweighted=True)
    if kind == 'bipy5':
        axis = np.array([0, 0, 1.])
    else:
        axis = next(a for k, a in axes(v, faces) if k == order and cell_on(pts, a) is not None)
    cores = [cell_on(pts, axis)] + ([cell_on(pts, -axis)] if ncores == 2 else [])
    step = 2 * np.pi / (players if ncores == 1 else order)
    rot = perm_of(pts, rotation(axis, step))
    gens = [rot]
    if players == 6:
        for _, a2 in all_axes(v, faces, 2):
            if abs(a2 @ axis) < 1e-9 and perm_of(pts, rotation(a2, np.pi)) is not None:
                gens.append(perm_of(pts, rotation(a2, np.pi))); break
    best = None
    for t in tris:
        if set(t) & set(cores):
            continue
        homes = orbit(t, gens)
        cells = [c for h in homes for c in h]
        if len(homes) != players or len(set(cells)) != 3 * players:
            continue
        h0 = list(t)
        pg = min(D[np.ix_(h0, list(h))].min() for h in homes if h != t) - 1
        cg = [D[h0, c].min() - 1 for c in cores]
        score = (min(pg, min(cg)), pg, -abs(cg[0] - cg[-1]))
        if best is None or score > best[0]:
            best = (score, homes, pg, cg)
    _, homes, pg, cg = best
    # every player's view is the same: check each home against the others
    for h in homes:
        others = [o for o in homes if o != h]
        assert min(D[np.ix_(list(h), list(o))].min() for o in others) - 1 == pg
        assert sorted(D[list(h), c].min() - 1 for c in cores) == sorted(cg)
    # rotate the axis onto +y
    y = np.array([0, 1., 0])
    c = np.cross(axis, y)
    R = np.eye(3) if np.linalg.norm(c) < 1e-12 else rotation(c, math.acos(np.clip(axis @ y, -1, 1)))
    pts = pts @ R.T
    # turn order: homes in order around the axis, so play goes round the globe
    az = lambda i: math.atan2(pts[i][2], pts[i][0])
    homes.sort(key=lambda h: math.atan2(*reversed(list(pts[list(h)].mean(axis=0)[[0, 2]]))))
    # rings by distance from the nearest core
    dist = D[:, cores].min(axis=1).astype(int)
    rings = {}
    for i in range(N):
        rings.setdefault(dist[i], []).append(i)
    name = {}
    for r, cells in rings.items():
        for k, i in enumerate(sorted(cells, key=lambda i: (round(az(i), 6), -pts[i][1]))):
            name[i] = (LETTERS[r], k)
    return dict(players=players, grid=f"{kind} GP({m},{n})", cells=N, ring_count=len(rings),
                player_gap=int(pg), core_gaps=[int(x) for x in cg],
                centers=[name[i] for i in cores],
                homes=[[name[i] for i in h] for h in homes],
                adjacencies={name[i]: sorted(name[j] for j in adj[i]) for i in range(N)},
                positions={name[i]: [round(float(x), 5) for x in pts[i]] for i in range(N)})

def edn_space(s):
    return f'["{s[0]}" {s[1]}]'

def edn(board):
    adj = "\n    ".join(f"{edn_space(s)} [{' '.join(edn_space(t) for t in ts)}]"
                       for s, ts in sorted(board['adjacencies'].items(), key=lambda kv: (kv[0][0], kv[0][1])))
    pos = "\n    ".join(f"{edn_space(s)} [{' '.join(repr(x) for x in p)}]"
                       for s, p in sorted(board['positions'].items(), key=lambda kv: (kv[0][0], kv[0][1])))
    return (f"   {board['players']}\n   {{:grid \"{board['grid']}\" :cells {board['cells']} :ring-count {board['ring_count']}\n"
            f"    :player-gap {board['player_gap']} :core-gaps {board['core_gaps']}\n"
            f"    :centers [{' '.join(edn_space(s) for s in board['centers'])}]\n"
            f"    :homes [{' '.join('[' + ' '.join(edn_space(s) for s in h) + ']' for h in board['homes'])}]\n"
            f"    :adjacencies\n    {{{adj}}}\n"
            f"    :positions\n    {{{pos}}}}}")

if __name__ == '__main__':
    boards = []
    for p in sorted(CHOICES):
        b = build(p)
        print(p, b['grid'], b['cells'], 'rings', b['ring_count'], 'gap', b['player_gap'], 'core', b['core_gaps'],
              'degrees', sorted({len(v) for v in b['adjacencies'].values()}), file=sys.stderr)
        boards.append(b)
    out = sys.argv[1]
    with open(out, 'w') as f:
        f.write(''';; GENERATED -- do not edit by hand. The SPHERE mutation's boards, one per
;; player count: a geodesic grid on the globe, hexagonal but for the few cells
;; the sphere forces to have fewer neighbours (Euler: they always total 12).
;; Made by dev/sphere/export_sphere.py (see docs/sphere-mutation.md):
;;   .venv/bin/python dev/sphere/export_sphere.py src/cljc/organism/sphere_boards.cljc
;;
;; Every board is symmetric about its core axis, so each player sees the same
;; board: the same open spaces to every neighbour and the same reach to each
;; core. Spaces are named as on the flat boards -- ring "A" is the core(s),
;; each later ring a step further out -- and :positions are unit vectors with
;; the first core at +y.
(ns organism.sphere-boards)

(def boards
  {
''')
        f.write("\n\n".join(edn(b) for b in boards))
        f.write("})\n")
