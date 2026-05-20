"""
Stage 3 — mesh2d: quality triangulation of the 2D region.

Delaunay refinement (Shewchuk's Triangle) with a minimum-angle guarantee. Two
modes:
  * uniform     — triangulate_region(region, edge)
  * metric      — triangulate_raw(...) then refine_raw(..., sizing): a second
                  Triangle pass with PER-TRIANGLE area constraints, so triangles
                  shrink where the lifted surface is steep and the lifted mesh
                  comes out ~uniform. (Triangle key `triangle_max_area`, opt `ra`.)

Triangle QUALITY lives here; the surface SHAPE is added later by lifting.
"""
from __future__ import annotations
import numpy as np
import triangle as tr


def base_area(edge):
    return (np.sqrt(3) / 4) * edge ** 2          # area of an equilateral triangle


def resample_closed(poly, edge):
    """Resample a closed polyline to ~`edge` arc-length spacing."""
    loop = np.vstack([poly, poly[:1]])
    seg = np.hypot(*np.diff(loop, axis=0).T)
    s = np.concatenate([[0], np.cumsum(seg)])
    total = s[-1]
    n = max(8, int(round(total / edge)))
    si = np.linspace(0, total, n, endpoint=False)
    return np.column_stack([np.interp(si, s, loop[:, 0]),
                            np.interp(si, s, loop[:, 1])])


def _pslg(region, edge):
    rings = [resample_closed(region.outer, edge)]
    rings += [resample_closed(h, edge) for h in region.holes]
    verts, segs = [], []
    for ring in rings:
        b = len(verts); m = len(ring)
        verts.extend(ring.tolist())
        segs.extend([[b + i, b + (i + 1) % m] for i in range(m)])
    A = {"vertices": np.array(verts), "segments": np.array(segs)}
    if region.holes:
        A["holes"] = np.array([_point_in_polygon_interior(h) for h in region.holes])
    return A


def unpack(t):
    V, T = t["vertices"], t["triangles"]
    vm = t.get("vertex_markers")
    boundary = (vm.flatten() == 1) if vm is not None else _infer_boundary(V, T)
    return V, T, boundary


def triangulate_raw(region, edge=1.0, min_angle=30.0):
    A = _pslg(region, edge)
    return tr.triangulate(A, f"pq{min_angle}a{base_area(edge):.6f}")


def refine_raw(t, sizing, min_angle=30.0):
    """Refine a prior triangulation `t` with a per-triangle max-area `sizing`
    callable: sizing(centroids Nx2) -> max_area per existing triangle."""
    cents = t["vertices"][t["triangles"]].mean(axis=1)
    t2 = dict(t)
    t2["triangle_max_area"] = np.asarray(sizing(cents), float).reshape(-1, 1)
    return tr.triangulate(t2, f"rpq{min_angle}a")


def triangulate_region(region, edge=1.0, min_angle=30.0):
    return unpack(triangulate_raw(region, edge, min_angle))


def tri_gradient_mag(V2, T, H):
    """|grad H| per triangle (H is piecewise-linear over the mesh). Closed-form
    2x2 solve of [e1; e2] . g = [dh1; dh2]."""
    P = V2[T]; h = H[T]
    e1 = P[:, 1] - P[:, 0]; e2 = P[:, 2] - P[:, 0]
    d1 = h[:, 1] - h[:, 0]; d2 = h[:, 2] - h[:, 0]
    det = e1[:, 0] * e2[:, 1] - e1[:, 1] * e2[:, 0]
    det = np.where(np.abs(det) < 1e-12, 1e-12, det)
    gx = (d1 * e2[:, 1] - e1[:, 1] * d2) / det
    gy = (e1[:, 0] * d2 - d1 * e2[:, 0]) / det
    return np.hypot(gx, gy)


def triangle_quality(V, T):
    """(min_angle_deg, mean_min_angle_deg, n_tris)."""
    P = V[T]
    mins = []
    for i in range(3):
        a = P[:, (i + 1) % 3] - P[:, i]
        b = P[:, (i + 2) % 3] - P[:, i]
        cos = np.einsum("ij,ij->i", a, b) / (
            np.linalg.norm(a, axis=1) * np.linalg.norm(b, axis=1) + 1e-12)
        mins.append(np.degrees(np.arccos(np.clip(cos, -1, 1))))
    m = np.min(np.array(mins), axis=0)
    return m.min(), m.mean(), len(T)


def _point_in_polygon_interior(poly):
    return poly.mean(axis=0)                      # adequate for our (hole-free) pieces


def _infer_boundary(V, T):
    from collections import Counter
    edges = Counter()
    for a, b, c in T:
        for e in ((a, b), (b, c), (c, a)):
            edges[(min(e), max(e))] += 1
    bset = {v for e, n in edges.items() if n == 1 for v in e}
    mask = np.zeros(len(V), bool); mask[list(bset)] = True
    return mask
