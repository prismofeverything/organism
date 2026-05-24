"""
Stage 2 — field: the smooth interior height-source over the region.

Solve the elastic-membrane / St-Venant torsion equation on the 2D mesh:

    -laplacian(u) = 1   inside R,    u = 0   on the boundary.

FEM with the cotangent stiffness K (PSD) and a lumped mass M:  K u = M 1.
The result is 0 on the rim, peaks on the medial axis, is C-infinity smooth, and
is a paraboloid over a disc / a parabolic ridge over a thin arm. Returns u
normalized to [0, 1].
"""
from __future__ import annotations
import numpy as np
import scipy.sparse as sp
import scipy.sparse.linalg as spla


def membrane_field(V, T, boundary):
    n = len(V)
    I, J, W = [], [], []
    mass = np.zeros(n)
    for a, b, c in T:
        Pa, Pb, Pc = V[a], V[b], V[c]
        for x, y, opp in ((a, b, c), (b, c, a), (c, a, b)):
            u = V[x] - V[opp]
            w = V[y] - V[opp]
            cross = u[0] * w[1] - u[1] * w[0]
            cot = np.dot(u, w) / (abs(cross) + 1e-12)
            half = 0.5 * cot
            I += [x, x, y, y]
            J += [x, y, x, y]
            W += [half, -half, -half, half]
        area = 0.5 * abs((Pb[0] - Pa[0]) * (Pc[1] - Pa[1]) -
                         (Pc[0] - Pa[0]) * (Pb[1] - Pa[1]))
        mass[[a, b, c]] += area / 3.0

    K = sp.csr_matrix((W, (I, J)), shape=(n, n))
    free = ~boundary
    u = np.zeros(n)
    Kff = K[free][:, free].tocsc()
    u[free] = spla.spsolve(Kff, mass[free])
    u = np.clip(u, 0, None)
    if u.max() > 0:
        u /= u.max()
    return u


def smooth_scalar(V, T, vals, boundary, iters=25, lam=0.5):
    """Uniform-Laplacian smoothing of a per-vertex scalar, pinning boundary
    vertices. Used to round the wall->dome shoulder (a fillet) and gently
    soften crests, while keeping H a single-valued height field (silhouette and
    moldability untouched)."""
    n = len(V)
    rows, cols = [], []
    for a, b, c in T:
        for x, y in ((a, b), (b, c), (c, a)):
            rows += [x, y]; cols += [y, x]
    A = sp.coo_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, n)).tocsr()
    A.data[:] = 1.0
    A = (A > 0).astype(float)
    deg = np.array(A.sum(1)).ravel(); deg[deg == 0] = 1.0
    out = np.asarray(vals, float).copy()
    free = ~boundary
    for _ in range(iters):
        avg = np.asarray(A @ out).ravel() / deg
        out[free] += lam * (avg[free] - out[free])
    return out
