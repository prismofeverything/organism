"""
MOVE connector graft via a PERIMETER LOFT (GRAFTING.md re-ranked Path 1).

MOVE can't use the height-field boss (build_graft.py): its connector is wider than
the body at the seat in every direction, so the cap must FLARE outward into an
overhang -- impossible for a single z=H(x,y). And its lower body is non-star
(hooked, r(theta) up to triple-valued), so the radial-ring builder collapses it.

A LOFT solves both. Key measured fact: MOVE's cross-section is HOOKED (non-star)
only below z~40; above that it is star-shaped (one r per angle). So:

    body  (z=0 .. FLARE_Z)     re-stack the REAL out/MOVE.obj slices as M-point
                               rings (arc length, anchored, symmetrized -> exact
                               3-fold). Preserves the hooks; watertight by loft.
    cap   (FLARE_Z .. seat)    blend each ring's radius toward the seat CIRCLE by
                               a smoothstep weight -> G1 at BOTH ends for free
                               (zero slope at the foot, horizontal at the seat).
                               This blend IS the outward flare.
    boss                       structured connector SoR (revolution_cap); its
                               outer ring IS the cap's seat ring (shared -> exact
                               weld, no boolean).
    bottom                     flat (v1) / socket (v2).

    uv run python pieces/build_move_graft.py
"""
from __future__ import annotations
import sys
from pathlib import Path
import numpy as np
import trimesh
import triangle as tr

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"; OUT.mkdir(exist_ok=True)
sys.path.insert(0, str(HERE / "meshlib"))

import connector_field as cf                              # noqa: E402
from build_graft import Builder as _Builder, revolution_cap, _level_fn  # noqa: E402

ZMAX = 60.0
FOLD = 3
M = 192                      # 3 * 2^6  -> every 2:1 belt level stays 3-fold
FLARE_Z = 46.0              # start of the flare (must be in the star-shaped zone, z>~40)
CONN_TARGET = 0.32          # connector ring target; forces seat ring to full M (level 0)
SOCK_TARGET = 0.30          # socket ring target; forces mouth ring to full M (level 0)
SOCK_WALL = 0.9             # min wall around the socket
N_BODY = 40                 # body slice count (z=0..FLARE_Z)
N_CAP = 16                  # cap rings (FLARE_Z..seat)
Z_HOLD = 4.5                # base bulge: hold full width up to here (socket dome top)
Z_BULGE = 16.0              # base bulge tapers to nothing by here


class Builder(_Builder):
    def ring_pts(self, pts2d, z):
        base = len(self.V)
        self.V.extend((float(p[0]), float(p[1]), float(z)) for p in pts2d)
        return list(range(base, len(self.V)))


# ----------------------------------------------------------------- slice utils
def _signed_area(P):
    x, y = P[:, 0], P[:, 1]
    return 0.5 * np.sum(x * np.roll(y, -1) - np.roll(x, -1) * y)


def slice_outline(mesh, z):
    """Largest closed loop of mesh ∩ {Z=z}, as a CCW (N,2) polyline (world x,y)."""
    sec = mesh.section(plane_origin=[0, 0, z], plane_normal=[0, 0, 1])
    if sec is None:
        raise SystemExit(f"no section at z={z}")
    loop = max(sec.discrete, key=len)[:, :2]
    if _signed_area(loop) < 0:
        loop = loop[::-1]
    return loop


def resample_arclen(loop, m):
    """`m` points equally spaced by arc length around the closed loop."""
    P = np.vstack([loop, loop[:1]])
    s = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(P, axis=0).T))])
    si = np.linspace(0, s[-1], m, endpoint=False)
    return np.column_stack([np.interp(si, s, P[:, 0]), np.interp(si, s, P[:, 1])])


def roll_anchor(pts, ang):
    """Roll so index 0 is the point whose angle is nearest `ang` (kills loft twist)."""
    th = np.arctan2(pts[:, 1], pts[:, 0])
    k = int(np.argmin(np.abs((th - ang + np.pi) % (2 * np.pi) - np.pi)))
    return np.roll(pts, -k, axis=0)


def symmetrize_ring(pts, fold):
    """Average each point with the rotated corresponding points of the other
    sectors -> exactly `fold`-fold (pts must be anchored, len divisible by fold)."""
    m = len(pts); step = m // fold
    out = np.zeros_like(pts)
    for k in range(fold):
        a = k * 2 * np.pi / fold; c, s = np.cos(a), np.sin(a)
        Rinv = np.array([[c, s], [-s, c]])       # Rot(-k*beta): overlay sector k onto 0
        out += (np.roll(pts, -k * step, axis=0) @ Rinv.T)
    return out / fold


def align_to(ref, cur):
    """Cyclically roll `cur` to best-match `ref` (min sum of squared index-wise
    distance). Removes loft twist on non-star (hooked) rings where angle-anchoring
    is ambiguous; propagated down the stack it keeps every ring registered."""
    best_k, best_d = 0, np.inf
    for k in range(len(cur)):
        d = float(np.sum((np.roll(cur, -k, axis=0) - ref) ** 2))
        if d < best_d:
            best_d, best_k = d, k
    return np.roll(cur, -best_k, axis=0)


def r_at_angle(loop, angles):
    """Radius of a STAR-shaped loop at the given angles (single-valued interp)."""
    th = np.arctan2(loop[:, 1], loop[:, 0]); r = np.hypot(loop[:, 0], loop[:, 1])
    o = np.argsort(th)
    return np.interp((angles + np.pi) % (2 * np.pi) - np.pi, th[o], r[o], period=2 * np.pi)


def smoothstep(t):
    t = np.clip(t, 0, 1)
    return t * t * (3 - 2 * t)


def base_floor(z):
    """Minimum body radius at height z so the (too-wide) socket fits with a wall:
    hold R_SOCK+wall up to the socket top, then taper to nothing -> a subtle base
    bulge that fills MOVE's pinched throats only where the socket needs room."""
    if z >= Z_BULGE:
        return 0.0
    t = max(0.0, (z - Z_HOLD) / (Z_BULGE - Z_HOLD))
    return (cf.R_SOCK + SOCK_WALL) * (1.0 - smoothstep(t))


def apply_base_bulge(pts, z):
    """Push ring points outward to at least base_floor(z), keeping their angle."""
    fl = base_floor(z)
    if fl <= 0:
        return pts
    rr = np.hypot(pts[:, 0], pts[:, 1])
    return pts * np.maximum(1.0, fl / np.maximum(rr, 1e-9))[:, None]


# ----------------------------------------------------------------- build
def build():
    mesh = trimesh.load(OUT / "MOVE.obj", process=False)
    seat_z = cf.seat_z(ZMAX)

    # foot ring at FLARE_Z defines the master angles theta_i (= connector angles)
    foot_loop = slice_outline(mesh, FLARE_Z)
    a0 = float(np.arctan2(*foot_loop[np.argmin(np.hypot(*foot_loop.T))][::-1]))  # a throat
    foot = symmetrize_ring(roll_anchor(resample_arclen(foot_loop, M), a0), FOLD)
    th_i = np.arctan2(foot[:, 1], foot[:, 0])
    cos_i, sin_i = np.cos(th_i), np.sin(th_i)

    b = Builder()

    # ---------- connector boss (peg); its outer ring is the seat ring ----------
    TH = th_i                                    # master angle set (foot order)
    lev = lambda L: TH[np.arange(0, M, 2 ** L)]
    level = _level_fn(CONN_TARGET, M)
    seat_ring, seat_L = revolution_cap(
        b, lev, level, lambda r: seat_z + cf.peg_height(r),
        ZMAX, cf.R_SEAT, CONN_TARGET, sign=+1)
    assert seat_L == 0, f"seat ring not full-M (level {seat_L}); lower CONN_TARGET"

    # ---------- cap: seat ring (z=seat_z, w=1) down to foot (z=FLARE_Z, w=0) ----
    prev = seat_ring
    for j in range(1, N_CAP + 1):
        if j == N_CAP:
            ring = b.ring_pts(foot, FLARE_Z)                   # foot = real section
        else:
            z = seat_z + (FLARE_Z - seat_z) * (j / N_CAP)
            w = smoothstep((z - FLARE_Z) / (seat_z - FLARE_Z))  # 1 at seat, 0 at foot
            Rb = r_at_angle(slice_outline(mesh, z), th_i)
            R = (1 - w) * Rb + w * cf.R_SEAT
            ring = b.ring_pts(np.column_stack([R * cos_i, R * sin_i]), z)
        b.loft(ring, prev)                       # prev above, ring below
        prev = ring

    # ---------- body: foot (FLARE_Z) down to z~0, real slices re-stacked --------
    # Align each ring to the one above (the foot is the reference) so the hooked
    # rings don't twist; symmetrize keeps each exactly 3-fold.
    zs = np.linspace(FLARE_Z, 0.0, N_BODY + 1)[1:]
    ref = foot
    for z in zs:
        zc = max(z, 0.4)
        r = symmetrize_ring(resample_arclen(slice_outline(mesh, zc), M), FOLD)
        r = align_to(ref, r)
        ref = r                                   # track BEFORE the bulge (real shape)
        r = apply_base_bulge(r, max(z, 0.0))      # widen the base to house the socket
        ring = b.ring_pts(r, 0.0 if z <= 0 else z)
        b.loft(ring, prev)
        prev = ring
    rim = prev
    rim_pts = np.array([b.V[i][:2] for i in rim])

    # ---------- bottom: socket cavity + flat annulus to the rim -----------------
    build_socket_bottom(b, rim, rim_pts, lev)
    return b.mesh()


def build_socket_bottom(b, rim, rim_pts, lev):
    """Socket cavity (centre dome+groove, oversized mirror of the peg) + a flat
    annulus (z=0) from the socket mouth out to the rim. The cavity is a structured
    SoR; the annulus is triangulated (rim is non-star), reusing the rim + mouth
    rings so it welds with no duplicate vertices."""
    level = _level_fn(SOCK_TARGET, M)
    cav, cav_L = revolution_cap(
        b, lev, level, cf.socket_height,
        float(cf.socket_height(np.array([0.0]))[0]), cf.R_SOCK, SOCK_TARGET, sign=-1)
    assert cav_L == 0, f"socket mouth not full-M (level {cav_L}); lower SOCK_TARGET"
    mouth_pts = np.array([b.V[i][:2] for i in cav])

    # flat annulus (z=0) mouth -> rim. MOVE is a SPIRAL, so the rim is rotationally
    # offset from the (foot-angle) mouth and the rim is hooked -> a forced index
    # loft twists. Triangulate the flat region instead (no forced correspondence),
    # reusing the rim + mouth rings so it welds with no duplicate vertices. The
    # interior Steiner points aren't 3-fold (bottom-only, sits on the board, does
    # not affect mating) -- accepted under the relaxed post-graft bar.
    V2 = np.vstack([rim_pts, mouth_pts])
    seg = np.array([[i, (i + 1) % M] for i in range(M)] +
                   [[M + i, M + (i + 1) % M] for i in range(M)])
    out = tr.triangulate({"vertices": V2, "segments": seg, "holes": [[0.0, 0.0]]},
                         "pYq28")
    VV, T = out["vertices"], out["triangles"]
    idmap = {k: rim[k] for k in range(M)}
    idmap.update({M + k: cav[k] for k in range(M)})
    for k in range(2 * M, len(VV)):
        idmap[k] = b.vert(VV[k, 0], VV[k, 1], 0.0)
    for (i, j, k) in T:
        b.F.append([idmap[i], idmap[k], idmap[j]])


def main():
    mesh = build()
    print(f"=== MOVE graft (loft) ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3  watertight={mesh.is_watertight} "
          f"euler={mesh.euler_number} zmax={mesh.vertices[:,2].max():.2f}")
    # symmetry check
    V = mesh.vertices; a = 2 * np.pi / FOLD; c, s = np.cos(a), np.sin(a)
    Vr = V @ np.array([[c, s, 0], [-s, c, 0], [0, 0, 1]])
    from scipy.spatial import cKDTree
    d, _ = cKDTree(V).query(Vr)
    print(f"  120deg symmetry mismatch: mean={d.mean():.3f} max={d.max():.3f} mm")
    out = OUT / "MOVE_graft.obj"; mesh.export(out); print(f"  wrote {out}")


if __name__ == "__main__":
    main()
