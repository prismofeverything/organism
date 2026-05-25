"""
Generative connector graft — EAT (GRAFTING.md approach F), fully-green build.

ONE watertight, exactly N-fold mesh, fully STRUCTURED (no boolean, no global
remesh -> connector mating surfaces exact). The key to clean triangles on a SHARP
star: a master angle set sampled by ARC LENGTH along the outline (dense at the
lobe tips, sparse at the throats), used for every ring, COARSENED toward each pole
by 2:1 decimation belts. Arc-length angles -> no long edges at the star points; a
single shared angle set -> aligned lofts; decimation -> no apex/bottom slivers.

    connector boss  concentric circle rings (dome+groove+ridge), apex-coarsened
    collar          star->circle loft (per-ray meridian) -> G1 seam, no shelf
    wall + bottom   star wall, flat bottom coarsened to the axis

EAT has no socket.   uv run python pieces/build_graft.py
"""
from __future__ import annotations
import sys
from pathlib import Path
import numpy as np
import trimesh

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"; OUT.mkdir(exist_ok=True)
sys.path.insert(0, str(HERE / "meshlib"))

from domain import load_region, round_corners                  # noqa: E402
from symmetry import symmetrize_region, _radial_outline, _r_at  # noqa: E402
from invariants import Spec, validate                          # noqa: E402
from solid import build_solid                                  # noqa: E402
import connector_field as cf                                   # noqa: E402

# per-piece: svg, fold, z_max, has_socket (food seats UNDER it), corner-round, collar knee,
# m = master angle count = fold * 2^k (so EVERY 2:1 belt level stays fold-symmetric).
SPECS = {
    "EAT":  dict(svg="eat.svg",  fold=5, zmax=48.0, socket=False, rnd=2.0, knee=0.70, m=160),
    "GROW": dict(svg="grow.svg", fold=4, zmax=36.0, socket=True,  rnd=2.0, knee=0.70, m=128),
    "MOVE": dict(svg="move.svg", fold=3, zmax=60.0, socket=True,  rnd=2.0, knee=0.70, m=192),
}
FULLNESS = 0.6
N_COL = 22           # collar meridian samples
TARGET = 0.7


def arc_angles(th_s, r_s, fold, m):
    """`m` angles sampled by PLANAR arc length along the outline (dense at lobes),
    exactly fold-fold symmetric (one wedge sampled, then rotated)."""
    beta = 2 * np.pi / fold; nw = m // fold
    alpha = float(th_s[np.argmin(r_s)])
    tt = np.linspace(alpha, alpha + beta, 4000)
    rr = _r_at(th_s, r_s, tt)
    P = np.column_stack([rr * np.cos(tt), rr * np.sin(tt)])
    s = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(P, axis=0).T))])
    tw = np.interp(np.linspace(0, s[-1], nw, endpoint=False), s, tt)
    full = np.concatenate([tw + k * beta for k in range(fold)])
    return np.sort(np.mod(full, 2 * np.pi))


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
    """Build a surface-of-revolution cap (apex/centre + concentric rings out to
    r_max), rings spaced by 3D ARC LENGTH (steep walls get enough rings) and the
    angular count COARSENED toward the centre by 2:1 belts (no fans/slivers).
    `height_fn(r)` -> z. `sign` orders the apex fan so the outward face is up
    (+1, peg) or down (-1, socket cavity). Returns the outer ring + its level."""
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


def build(name):
    sp = SPECS[name]; fold, zmax, knee = sp["fold"], sp["zmax"], sp["knee"]
    reg = round_corners(symmetrize_region(
        load_region(HERE / "inputs" / sp["svg"], name), fold), radius=sp["rnd"])
    th_s, r_s = _radial_outline(reg)
    fz = cf.seat_z(zmax); z_knee = knee * fz
    TH = arc_angles(th_s, r_s, fold, sp["m"])
    lev = lambda L: TH[::2 ** L]
    level = _level_fn(TARGET, sp["m"])
    M = sp["m"]
    b = Builder()

    # ---------- connector boss (peg) on top ----------
    seat, seat_L = revolution_cap(b, lev, level, lambda r: fz + cf.peg_height(r),
                                  zmax, cf.R_SEAT, TARGET, sign=+1)

    # ---------- collar: seat -> wall top (star->circle morph) ----------
    R0 = _r_at(th_s, r_s, TH)
    mer = np.stack([cf.collar_meridian(R0[i], fz, knee, FULLNESS, n=N_COL)
                    for i in range(M)], 0)

    def collar_ring(ang, k):
        R0a = _r_at(th_s, r_s, ang)
        p = np.array([cf.collar_meridian(R0a[i], fz, knee, FULLNESS, N_COL)[k]
                      for i in range(len(ang))])
        return b.ring(ang, p[:, 0], p[:, 1])

    prev = seat; curL = seat_L; k = 1
    while curL > 0 and k < N_COL:
        cur = collar_ring(lev(curL - 1), k); b.belt(cur, prev); prev = cur; curL -= 1; k += 1
    for kk in range(k, N_COL):
        cur = b.ring(TH, mer[:, kk, 0], mer[:, kk, 1]); b.loft(prev, cur); prev = cur

    # ---------- wall: walltop -> z=0 ----------
    nz = max(1, int(round(z_knee / TARGET)))
    for s in range(1, nz + 1):
        cur = b.ring(TH, R0, z_knee * (1 - s / nz)); b.loft(prev, cur); prev = cur
    rim = prev

    # ---------- bottom ----------
    if sp["socket"]:
        build_socket_bottom(b, th_s, r_s, lev, level, rim, R0)
    else:
        build_flat_bottom(b, th_s, r_s, lev, level, rim, R0)
    return b.mesh(), reg


def build_flat_bottom(b, th_s, r_s, lev, level, rim, R0):
    """Flat star bottom: rim -> centre, halving the angle set toward the axis."""
    prev = rim; L = 0
    nr = max(2, int(round(float(R0.max()) / TARGET)))
    for s in range(1, nr):
        frac = 1 - s / nr
        if L < 4 and R0.max() * frac < 0.6 * TARGET * len(lev(L)) / (2 * np.pi):
            cur = b.ring(lev(L + 1), _r_at(th_s, r_s, lev(L + 1)) * frac, 0.0)
            b.belt(prev, cur); L += 1
        else:
            cur = b.ring(lev(L), _r_at(th_s, r_s, lev(L)) * frac, 0.0); b.loft(cur, prev)
        prev = cur
    cen = b.vert(0.0, 0.0, 0.0)
    for k in range(len(prev)):
        b.F.append([prev[k], cen, prev[(k + 1) % len(prev)]])


def build_socket_bottom(b, th_s, r_s, lev, level, rim, R0):
    """SOCKET cavity (centre -> mouth R_SOCK, ceiling rising into the body =
    oversized mirror of the peg) + a flat star annulus (mouth -> star rim, z=0).
    The annulus connects DIRECTLY to the cavity's outer ring (no duplicate ring)."""
    Rs = cf.R_SOCK
    cav, cav_L = revolution_cap(b, lev, level, cf.socket_height,
                                cf.socket_height(np.array([0.0]))[0], Rs, TARGET, sign=-1)
    # flat annulus: cavity mouth (level cav_L) -> star rim (level 0), z=0. Build
    # inward (rim -> mouth) widening nothing; we go OUTWARD from `cav` so each belt
    # DOUBLES the angle set toward the rim (cav_L -> 0). Last ring welds to `rim`.
    prev = cav; L = cav_L
    nr = max(2, int(round((float(R0.max()) - Rs) / TARGET)))
    for s in range(1, nr):
        frac = 1 - s / nr                                # 1 at mouth -> 0 at rim
        rad_here = frac * Rs + (1 - frac) * float(R0.max())
        if L > 0 and rad_here > 1.2 * TARGET * len(lev(L)) / (2 * np.pi):
            L -= 1                                        # double toward the rim
            rad = frac * Rs + (1 - frac) * _r_at(th_s, r_s, lev(L))
            cur = b.ring(lev(L), rad, 0.0); b.belt(cur, prev)
        else:
            rad = frac * Rs + (1 - frac) * _r_at(th_s, r_s, lev(L))
            cur = b.ring(lev(L), rad, 0.0); b.loft(prev, cur)
        prev = cur
    # final ring -> the star rim (both level 0, TH)
    if L == 0:
        b.loft(prev, rim)
    else:
        raise SystemExit(f"socket annulus didn't widen to rim level (stuck at {L})")


def main():
    names = [a for a in sys.argv[1:] if a in SPECS] or list(SPECS)
    for name in names:
        sp = SPECS[name]
        mesh, reg = build(name)
        spec = Spec(name, sp["zmax"], "hemisphere", 7 / 8, 1.0, fold=sp["fold"])
        print(f"\n=== {name} graft ===  {len(mesh.vertices)} verts, "
              f"{len(mesh.faces)} faces, vol={mesh.volume:.0f}mm^3  "
              f"watertight={mesh.is_watertight} euler={mesh.euler_number}")
        validate(mesh, reg, spec)
        out = OUT / f"{name}_graft.obj"
        mesh.export(out)
        print(f"  wrote {out}")


if __name__ == "__main__":
    main()
