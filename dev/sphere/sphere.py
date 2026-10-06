"""Search geodesic sphere boards for fair N-player layouts.

Grid: Goldberg-style GP(m,n) subdivision of an icosahedron (12 five-neighbour
cells) or octahedron (6 four-neighbour cells); every other cell has six.
Core: one cell on a k-fold rotation axis, with N dividing k, so a rotation
about the core carries each player's home to the next: exact fairness.
Home: a triangle of three mutually adjacent cells.
Gaps count OPEN spaces between, like the flat boards (distance - 1).
"""
import itertools, sys
import numpy as np
from scipy.spatial import ConvexHull
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import shortest_path

def icosahedron():
    p = (1 + 5 ** .5) / 2
    v = [(-1, p, 0), (1, p, 0), (-1, -p, 0), (1, -p, 0), (0, -1, p), (0, 1, p),
         (0, -1, -p), (0, 1, -p), (p, 0, -1), (p, 0, 1), (-p, 0, -1), (-p, 0, 1)]
    return np.array(v, float)

def octahedron():
    return np.array([(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)], float)

def faces_of(v):
    v = v / np.linalg.norm(v, axis=1)[:, None]
    f = []
    for s in ConvexHull(v).simplices:
        a, b, c = v[s]
        if np.dot(np.cross(b - a, c - a), a) < 0:
            s = s[[0, 2, 1]]
        f.append(s)
    return v, f

def gp(base, m, n):
    v, faces = faces_of(base)
    e1, e2 = np.array([1., 0]), np.array([.5, 3 ** .5 / 2])
    p1 = m * e1 + n * e2
    rot = np.array([[.5, -3 ** .5 / 2], [3 ** .5 / 2, .5]])
    p2 = rot @ p1
    M = np.array([p1, p2]).T
    Minv = np.linalg.inv(M)
    pts = []
    R = m + n + 1
    for i in range(-R, 2 * R):
        for j in range(-R, 2 * R):
            q = i * e1 + j * e2
            wb, wc = Minv @ q
            wa = 1 - wb - wc
            if min(wa, wb, wc) < -1e-9:
                continue
            for a, b, c in faces:
                x = wa * v[a] + wb * v[b] + wc * v[c]
                pts.append(x / np.linalg.norm(x))
    pts = np.array(pts)
    keep = []
    for x in pts:
        if not any(np.dot(x, y) > 1 - 1e-9 for y in keep):
            keep.append(x)
    pts = np.array(keep)
    hull = ConvexHull(pts)
    edges = set()
    for s in hull.simplices:
        for a, b in itertools.combinations(sorted(s), 2):
            edges.add((a, b))
    adj = [[] for _ in pts]
    for a, b in edges:
        adj[a].append(b); adj[b].append(a)
    tris = [tuple(sorted(s)) for s in hull.simplices]
    return pts, adj, tris, v, faces

def rotation(axis, angle):
    axis = axis / np.linalg.norm(axis)
    K = np.array([[0, -axis[2], axis[1]], [axis[2], 0, -axis[0]], [-axis[1], axis[0], 0]])
    return np.eye(3) + np.sin(angle) * K + (1 - np.cos(angle)) * K @ K

def axes(v, faces):
    """(order, unit axis) for each symmetry axis class of the base solid."""
    deg = len(faces) * 3 // len(v)  # faces meeting at a vertex
    out = [(deg, v[0])]
    a, b, c = faces[0]
    out.append((3, (v[a] + v[b] + v[c]) / 3))
    out.append((2, (v[a] + v[b]) / 2))
    return [(k, x / np.linalg.norm(x)) for k, x in out]

def search(players, kind, m, n):
    base = icosahedron() if kind == 'ico' else octahedron()
    pts, adj, tris, v, faces = gp(base, m, n)
    N = len(pts)
    rows = [a for a in range(N) for _ in adj[a]]
    cols = [b for a in range(N) for b in adj[a]]
    D = shortest_path(csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(N, N)), unweighted=True)
    defects = {i for i in range(N) if len(adj[i]) != 6}
    best = None
    for order, axis in axes(v, faces):
        if order % players:
            continue
        hit = np.where(pts @ axis > 1 - 1e-9)[0]
        if not len(hit):
            continue  # no cell sits on this axis at this size
        core = hit[0]
        Rm = rotation(axis, 2 * np.pi / players)
        img = pts @ Rm.T
        perm = np.argmax(img @ pts.T, axis=1)
        if not np.allclose(pts[perm], img, atol=1e-6):
            continue  # this rotation is not a symmetry of the grid (chirality)
        seen = set()
        for t in tris:
            if core in t or t in seen:
                continue
            homes = [t]
            for _ in range(players - 1):
                homes.append(tuple(sorted(perm[list(homes[-1])])))
            for h in homes:
                seen.add(h)
            cells = [c for h in homes for c in h]
            if len(set(cells)) != 3 * players:
                continue
            h0 = list(homes[0])
            pg = min(D[np.ix_(h0, list(h))].min() for h in homes[1:]) - 1
            cg = D[h0, core].min() - 1
            touch = bool(set(cells) & defects)
            score = (min(pg, cg), pg, -touch)
            if best is None or score > best[0]:
                best = (score, dict(cells=N, grid=f"{kind} GP({m},{n})", axis=order,
                                    core_neighbours=len(adj[core]), player_gap=int(pg),
                                    core_gap=int(cg), home_on_defect=touch))
    return best[1] if best else None

if __name__ == '__main__':
    sizes = sorted({(m, n) for m in range(1, 9) for n in range(0, m + 1)},
                   key=lambda mn: mn[0] ** 2 + mn[0] * mn[1] + mn[1] ** 2)
    for players in [2, 3, 4, 5]:
        print(f"== {players} players")
        results = []
        for kind in ['ico', 'oct']:
            for m, n in sizes:
                T = m * m + m * n + n * n
                cells = (10 if kind == 'ico' else 4) * T + 2
                if cells > 700:
                    continue
                r = search(players, kind, m, n)
                if r:
                    results.append(r)
        results.sort(key=lambda r: r['cells'])
        shown = 0
        for r in results:
            if r['player_gap'] >= 2:
                print(" ", r)
                shown += 1
            if r['player_gap'] >= 3 and r['core_gap'] >= 3:
                break
        sys.stdout.flush()
