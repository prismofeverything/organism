"""
Ring-based mesh primitives used to build the universal connector graft:

    Builder        ring/loft/belt mesh accumulator (-> trimesh)
    revolution_cap apex + concentric rings with 2:1 belt coarsening
    _level_fn      target ring-count -> belt level by radius

Originally lived in pieces/build_graft.py (its CLI is superseded by
meshlib/build.py's build_graft); kept here as the reusable library piece.
"""
from __future__ import annotations
import numpy as np
import trimesh


class Builder:
    def __init__(self): self.V, self.F = [], []

    def ring(self, ang, radius, z):
        ang = np.asarray(ang); n = len(ang)
        radius = np.broadcast_to(radius, (n,)); z = np.broadcast_to(z, (n,))
        base = len(self.V)
        self.V.extend(np.column_stack([radius * np.cos(ang), radius * np.sin(ang), z]))
        return list(range(base, len(self.V)))

    def vert(self, x, y, z): self.V.append((x, y, z)); return len(self.V) - 1

    def loft(self, A, B):                          # equal-length rings, shorter 3D diagonal
        V = self.V; n = len(A)
        for i in range(n):
            j = (i + 1) % n
            if np.linalg.norm(np.subtract(V[A[i]], V[B[j]])) <= \
               np.linalg.norm(np.subtract(V[A[j]], V[B[i]])):
                self.F += [[A[i], A[j], B[j]], [A[i], B[j], B[i]]]
            else:
                self.F += [[A[i], A[j], B[i]], [A[j], B[j], B[i]]]

    def belt(self, O, I):                          # outer 2M -> inner M, O[2k]~I[k]
        Mi = len(I)
        for k in range(Mi):
            a, b, c = O[2 * k], O[2 * k + 1], O[(2 * k + 2) % (2 * Mi)]
            self.F += [[a, b, I[k]], [b, c, I[k]], [c, I[(k + 1) % Mi], I[k]]]

    def mesh(self):
        m = trimesh.Trimesh(vertices=np.array(self.V, float),
                            faces=np.array(self.F, int), process=True)
        trimesh.repair.fix_normals(m)
        return m


def _level_fn(target, m):
    def level(r):
        return int(np.clip(round(np.log2(m * target / (2 * np.pi * max(r, 1e-3)))), 0, 5))
    return level


def revolution_cap(b, lev, level, height_fn, apex_z, r_max, target, sign=+1):
    """Surface-of-revolution cap (apex + concentric rings out to r_max), rings
    spaced by 3D ARC LENGTH so steep walls get enough rings, angular count
    COARSENED toward the centre by 2:1 belts (no fans/slivers). `height_fn(r)`
    -> z. `sign` orders the apex fan so the outward face is up (+1, peg) or
    down (-1, socket cavity). Returns the outer ring + its level."""
    rf = np.linspace(0.0, r_max, 3000); zf = height_fn(rf)
    sc = np.concatenate([[0], np.cumsum(np.hypot(np.diff(rf), np.diff(zf)))])
    n_rings = max(4, int(round(sc[-1] / (0.7 * target))))
    r_rings = np.interp(np.linspace(0, sc[-1], n_rings + 1), sc, rf)
    r_rings = r_rings[r_rings > 0.18]
    Ls = [level(r) for r in r_rings]
    for i in range(1, len(Ls)):
        Ls[i] = max(min(Ls[i], Ls[i - 1]), Ls[i - 1] - 1)
    rings = [b.ring(lev(L), r, height_fn(np.array([r]))[0]) for r, L in zip(r_rings, Ls)]
    apex = b.vert(0.0, 0.0, apex_z)
    inner = rings[0]
    for k in range(len(inner)):
        tri = [apex, inner[k], inner[(k + 1) % len(inner)]]
        b.F.append(tri if sign > 0 else tri[::-1])
    for i in range(1, len(rings)):
        if Ls[i - 1] == Ls[i]:
            b.loft(rings[i - 1], rings[i])
        elif Ls[i - 1] - Ls[i] == 1:
            b.belt(rings[i], rings[i - 1])
        else:
            raise SystemExit(f"level jump >1 ({Ls[i-1]}->{Ls[i]}) at r={r_rings[i]:.2f}")
    return rings[-1], Ls[-1]
