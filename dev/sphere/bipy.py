"""Search pentagonal-bipyramid grids: D5-symmetric, two pentagon poles for the
cores and five four-neighbour cells round the equator."""
import numpy as np, sys
from sphere import gp, rotation
from sphere_boards import perm_of, cell_on, orbit
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import shortest_path
def bipyramid(k, h):
    eq = [(np.cos(2*np.pi*i/k), np.sin(2*np.pi*i/k), 0) for i in range(k)]
    return np.array(eq + [(0, 0, h), (0, 0, -h)], float)
def layouts(k, players, m, n, h=1.0):
    pts, adj, tris, v, faces = gp(bipyramid(k, h), m, n)
    N = len(pts)
    rows = [a for a in range(N) for _ in adj[a]]; cols = [b for a in range(N) for b in adj[a]]
    D = shortest_path(csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(N, N)), unweighted=True)
    z = np.array([0, 0, 1.])
    cores = [cell_on(pts, z), cell_on(pts, -z)]
    rot = perm_of(pts, rotation(z, 2*np.pi/players))
    if None in cores or rot is None: return None
    gens = [rot]
    flip = perm_of(pts, rotation(np.array([1., 0, 0]), np.pi))
    best = None
    for t in tris:
        if set(t) & set(cores): continue
        homes = orbit(t, gens)
        if len(homes) != players: continue
        cells = [c for hh in homes for c in hh]
        if len(set(cells)) != 3*players: continue
        h0 = list(t)
        pg = min(D[np.ix_(h0, list(hh))].min() for hh in homes if hh != t) - 1
        cg = [D[h0, c].min() - 1 for c in cores]
        score = (min(pg, min(cg)), pg, -abs(cg[0]-cg[1]))
        if best is None or score > best[0]:
            best = (score, dict(cells=N, grid=('bipy%d' % k, m, n), player_gap=int(pg), core_gaps=[int(x) for x in cg],
                                degrees=sorted(set(len(a) for a in adj)), flip_symmetric=flip is not None))
    return best and best[1]
if __name__ == '__main__':
    for m in range(1, 8):
        for n in range(0, m+1):
            r = layouts(5, 5, m, n)
            if r and r['player_gap'] >= 2: print(r); sys.stdout.flush()
