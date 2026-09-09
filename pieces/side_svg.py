"""Side elevation of each piece -> vector SVG: a filled silhouette and a line trace.

Everything comes off the real print mesh, never a raster trace. Two view-dependent line
sets are found in 3D -- the occluding contour (where the smooth surface turns away from
the camera, interpolated on vertex normals, so it is not stuck to triangle edges) and
creases (sharp dihedral edges) -- and hidden stretches are removed with an exact
point-in-projected-triangle depth test. Every polyline, outline included, is then
resampled by arclength, smoothed with corners pinned, and refit as cubic Beziers, so the
curves describe the form instead of the triangle mesh that happens to approximate it.

    ../.venv/bin/python side_svg.py             # EAT MOVE GROW at az 0 -> svg/
    ../.venv/bin/python side_svg.py 90 GROW     # one piece, another azimuth
"""
import math, os, sys
import numpy as np
import shapely
import trimesh
from scipy.ndimage import gaussian_filter1d
from scipy.spatial import cKDTree
from shapely.geometry import Polygon
from shapely.geometry.polygon import orient

ROOT = os.path.dirname(os.path.abspath(__file__))
OUTDIR = os.path.join(ROOT, "svg")

GRID       = 0.005  # mm  union snap: robustness only, the smoothing pass removes it
HOLE_MIN   = 0.25   # mm2 smaller "interiors" of the union are artifacts, not real voids
STEP       = 0.06   # mm  arclength resample pitch
SIGMA      = 0.12   # mm  gaussian smoothing width along arclength (kills mesh facet noise)
CORNER_WIN = 0.45   # mm  how far the corner test looks either side
CORNER_ANG = 38.0   # deg turn across that window that counts as a corner
TOL_OUT    = 0.05   # mm  curve fit tolerance, silhouette
TOL_LINE   = 0.09   # mm  curve fit tolerance, interior lines
CREASE_ANG = 30.0   # deg dihedral angle that reads as a crease
BIAS       = 0.8    # mm  depth slack in the hidden-line test (grazing faces self-occlude)
MIN_RUN    = 1.8    # mm  drop visible fragments shorter than this (they read as lint)
ON_EDGE    = 0.15   # mm  interior detail this close to the outline *is* the outline
PAD        = 0.25   # mm  viewBox padding so strokes are not clipped
GAP        = 5.0    # mm  spacing between pieces on the combined sheets
W_OUT, W_LINE = 0.30, 0.16   # mm stroke weights


def load(name):
    for p in (f"{ROOT}/stl/{name}.stl", f"{ROOT}/out/{name}_sculpt_graft.obj"):
        if os.path.exists(p):
            return trimesh.load(p, force="mesh"), os.path.relpath(p, ROOT)
    raise SystemExit(f"no mesh for {name} (make grafts, then flatten_pieces.sh)")


def basis(az_deg):
    a = math.radians(az_deg)
    return (np.array([-math.sin(a), math.cos(a), 0.0]),   # right: screen +x
            np.array([math.cos(a), math.sin(a), 0.0]),    # view: toward the camera
            np.array([0.0, 0.0, 1.0]))                    # up


# ---------------------------------------------------------------- silhouette

def silhouette(mesh, right, view, up):
    """Filled outline: front faces alone cover a closed solid, so union just those."""
    tri = mesh.vertices[mesh.faces[mesh.face_normals @ view > 0]]
    pts = np.dstack([tri @ right, tri @ up])
    ring = np.concatenate([pts, pts[:, :1]], axis=1).reshape(-1, 2)
    tris = shapely.polygons(shapely.linearrings(ring, indices=np.repeat(np.arange(len(pts)), 4)))
    u = shapely.union_all(shapely.make_valid(tris), grid_size=GRID)
    polys = [g for g in getattr(u, "geoms", [u]) if g.geom_type == "Polygon"]
    p = max(polys, key=lambda g: g.area)
    return orient(Polygon(p.exterior, [r for r in p.interiors if Polygon(r).area >= HOLE_MIN]))


# ------------------------------------------------------------ line extraction

def _chain(links, pos):
    """(n,2) node pairs -> point arrays. Open chains first so loops do not swallow them."""
    adj = {}
    for i, (a, b) in enumerate(links):
        adj.setdefault(a, []).append((b, i)); adj.setdefault(b, []).append((a, i))
    used = np.zeros(len(links), bool)
    out = []
    for ends in (True, False):
        for n in list(adj):
            if ends and len(adj[n]) != 1:
                continue
            while any(not used[i] for _, i in adj[n]):
                path, cur = [n], n
                while True:
                    nxt = [(o, i) for o, i in adj[cur] if not used[i]]
                    if not nxt:
                        break
                    o, i = nxt[0]; used[i] = True; path.append(o); cur = o
                    if o == n:
                        break
                if len(path) > 1:
                    out.append(pos[path])
    return out


def occluding(mesh, view):
    """Where the smooth surface turns away: n.v = 0, interpolated along each mesh edge."""
    g = mesh.vertex_normals @ view
    E = mesh.edges_unique
    sc = (g[E[:, 0]] > 0) != (g[E[:, 1]] > 0)
    ga, gb = g[E[sc, 0]], g[E[sc, 1]]
    t = (ga / (ga - gb))[:, None]
    pos = mesh.vertices[E[sc, 0]] * (1 - t) + mesh.vertices[E[sc, 1]] * t
    eid = np.full(len(E), -1); eid[np.where(sc)[0]] = np.arange(sc.sum())
    hit = sc[mesh.faces_unique_edges]
    two = np.where(hit.sum(1) == 2)[0]                    # a face the contour crosses once
    return _chain(eid[mesh.faces_unique_edges[two]][hit[two]].reshape(-1, 2), pos)


def creases(mesh, view):
    """Sharp edges with at least one face turned toward the camera."""
    front = mesh.face_normals @ view > 0
    fa = mesh.face_adjacency
    keep = ((np.degrees(mesh.face_adjacency_angles) > CREASE_ANG)
            & (front[fa[:, 0]] | front[fa[:, 1]]))
    return _chain(mesh.face_adjacency_edges[keep], mesh.vertices)


class Occluder:
    """Hidden-line test: is anything of the solid nearer the camera at this screen point?"""

    def __init__(self, mesh, right, view, up):
        self.right, self.view, self.up = right, view, up
        tri = mesh.vertices[mesh.faces[mesh.face_normals @ view > 0]]
        self.A = np.dstack([tri @ right, tri @ up])       # (n,3,2) screen
        self.D = tri @ view                               # (n,3) depth
        c = self.A.mean(1)
        self.tree = cKDTree(c)
        self.r = np.linalg.norm(self.A - c[:, None], axis=2).max() + 1e-6

    def to2d(self, P):
        return np.column_stack([P @ self.right, P @ self.up]), P @ self.view

    def visible(self, P):
        q, dq = self.to2d(P)
        cand = self.tree.query_ball_point(q, self.r)      # every triangle that can cover q
        n = np.fromiter(map(len, cand), int, len(cand))
        out = np.ones(len(q), bool)
        if not n.sum():
            return out
        qi = np.repeat(np.arange(len(q)), n)
        ti = np.concatenate(cand)
        a, b, c = self.A[ti, 0], self.A[ti, 1], self.A[ti, 2]
        v0, v1, v2 = b - a, c - a, q[qi] - a
        den = v0[:, 0] * v1[:, 1] - v1[:, 0] * v0[:, 1]
        den = np.where(np.abs(den) < 1e-12, 1e-12, den)
        u = (v2[:, 0] * v1[:, 1] - v1[:, 0] * v2[:, 1]) / den
        v = (v0[:, 0] * v2[:, 1] - v2[:, 0] * v0[:, 1]) / den
        d = self.D[ti, 0] * (1 - u - v) + self.D[ti, 1] * u + self.D[ti, 2] * v
        occ = (u >= -1e-9) & (v >= -1e-9) & (u + v <= 1 + 1e-9) & (d > dq[qi] + BIAS)
        np.logical_and.at(out, qi[occ], False)
        return out


def visible_runs(lines, occ, ring=None):
    """3D polylines -> the screen-space stretches that are actually in view."""
    out = []
    for L in lines:
        if len(L) < 2:
            continue
        xy, _ = occ.to2d(L)
        keep = occ.visible(L)
        if ring is not None:                              # do not redraw the outline
            keep &= shapely.distance(shapely.points(xy), ring) > ON_EDGE
        start = None
        for i, k in enumerate(list(keep) + [False]):
            if k and start is None:
                start = i
            elif not k and start is not None:
                run = xy[start:i]
                if len(run) > 1 and np.linalg.norm(np.diff(run, axis=0), axis=1).sum() >= MIN_RUN:
                    out.append(run)
                start = None
    return out


# ------------------------------------------------------- polyline -> curves

def resample(P, closed):
    if closed and not np.allclose(P[0], P[-1]):
        P = np.vstack([P, P[:1]])
    P = P[np.concatenate([[True], np.linalg.norm(np.diff(P, axis=0), axis=1) > 1e-9])]
    if len(P) < 3:
        return P
    s = np.concatenate([[0], np.cumsum(np.linalg.norm(np.diff(P, axis=0), axis=1))])
    t = np.linspace(0, s[-1], max(int(round(s[-1] / STEP)), 8) + 1)
    if closed:
        t = t[:-1]
    return np.column_stack([np.interp(t, s, P[:, k]) for k in range(P.shape[1])])


def corner_mask(P, closed):
    """A corner is a turn held across ~CORNER_WIN, not a single noisy vertex."""
    n, k = len(P), max(1, int(round(CORNER_WIN / STEP)))
    if n < 2 * k + 3:
        return np.zeros(n, bool)
    i = np.arange(n)
    ia = (i - k) % n if closed else np.clip(i - k, 0, n - 1)
    ib = (i + k) % n if closed else np.clip(i + k, 0, n - 1)
    v1, v2 = P - P[ia], P[ib] - P
    cos = (v1 * v2).sum(1) / np.maximum(np.linalg.norm(v1, axis=1) * np.linalg.norm(v2, axis=1), 1e-12)
    turn = np.degrees(np.arccos(np.clip(cos, -1, 1)))
    if not closed:
        turn[:k] = turn[-k:] = 0
    out = np.zeros(n, bool)
    for j in np.where(turn > CORNER_ANG)[0]:              # keep only the peak of each cluster
        w = np.arange(j - k, j + k + 1)
        w = w % n if closed else np.clip(w, 0, n - 1)
        out[j] = turn[j] >= turn[w].max() - 1e-9
    return out


def smooth(P, closed, cor):
    """Gaussian along arclength, ramped to zero at corners so they stay sharp."""
    sig = SIGMA / STEP
    if closed and not cor.any():
        return gaussian_filter1d(P, sig, axis=0, mode="wrap")
    if closed:
        s = int(np.argmax(cor))
        R = smooth(np.vstack([np.roll(P, -s, axis=0), P[s:s + 1]]),
                   False, np.append(np.roll(cor, -s), True))
        return np.roll(R[:-1], s, axis=0)
    out, k = P.copy(), max(1, int(2 * sig))
    bounds = [0] + [i for i in np.where(cor)[0] if 0 < i < len(P) - 1] + [len(P) - 1]
    for a, b in zip(bounds[:-1], bounds[1:]):
        sp = P[a:b + 1]
        if len(sp) < 3:
            continue
        m = len(sp)
        w = np.clip(np.minimum(np.arange(m), m - 1 - np.arange(m)) / k, 0, 1)[:, None]
        out[a:b + 1] = sp * (1 - w) + gaussian_filter1d(sp, sig, axis=0, mode="nearest") * w
    return out


def _bez(seg, t):
    p0, p1, p2, p3 = seg
    t = t[:, None]
    return ((1 - t) ** 3) * p0 + 3 * ((1 - t) ** 2) * t * p1 + 3 * (1 - t) * t ** 2 * p2 + (t ** 3) * p3


def _generate(P, u, t1, t2):
    """Least-squares cubic through the span ends with the tangents held fixed."""
    b1, b2 = 3 * u * (1 - u) ** 2, 3 * u ** 2 * (1 - u)
    A1, A2 = t1[None, :] * b1[:, None], t2[None, :] * b2[:, None]
    rhs = P - (P[0] * ((1 - u) ** 3 + b1)[:, None] + P[-1] * (b2 + u ** 3)[:, None])
    c11, c12, c22 = (A1 * A1).sum(), (A1 * A2).sum(), (A2 * A2).sum()
    x1, x2 = (A1 * rhs).sum(), (A2 * rhs).sum()
    det, chord = c11 * c22 - c12 * c12, np.linalg.norm(P[-1] - P[0])
    a1 = a2 = chord / 3
    if abs(det) > 1e-12:
        a1, a2 = (x1 * c22 - x2 * c12) / det, (c11 * x2 - c12 * x1) / det
        if a1 < 1e-6 * chord or a2 < 1e-6 * chord:
            a1 = a2 = chord / 3
    return (P[0], P[0] + t1 * a1, P[-1] + t2 * a2, P[-1])


def _reparam(P, seg, u):
    """One Newton step of each sample toward its closest point on the curve."""
    p0, p1, p2, p3 = seg
    t = u[:, None]
    q = _bez(seg, u)
    d1 = 3 * (1 - t) ** 2 * (p1 - p0) + 6 * (1 - t) * t * (p2 - p1) + 3 * t ** 2 * (p3 - p2)
    d2 = 6 * (1 - t) * (p2 - 2 * p1 + p0) + 6 * t * (p3 - 2 * p2 + p1)
    den = (d1 * d1).sum(1) + ((q - P) * d2).sum(1)
    return np.clip(u - np.where(np.abs(den) < 1e-12, 0.0, ((q - P) * d1).sum(1) / np.where(np.abs(den) < 1e-12, 1, den)), 0, 1)


def _fit_span(P, t1, t2, tol, depth=0):
    """Schneider: fit one cubic, reparameterise, and split at the worst point if needed."""
    if len(P) < 3:
        d = np.linalg.norm(P[-1] - P[0]) / 3
        return [(P[0], P[0] + t1 * d, P[-1] + t2 * d, P[-1])]
    s = np.concatenate([[0], np.cumsum(np.linalg.norm(np.diff(P, axis=0), axis=1))])
    u = s / s[-1] if s[-1] > 0 else s
    seg = _generate(P, u, t1, t2)
    err = np.linalg.norm(_bez(seg, u) - P, axis=1)
    if err.max() > tol and depth < 24:
        for _ in range(4):
            u = _reparam(P, seg, u)
            seg = _generate(P, u, t1, t2)
        err = np.linalg.norm(_bez(seg, u) - P, axis=1)
    if err.max() <= tol or depth >= 24:
        return [seg]
    i = min(max(int(np.argmax(err)), 1), len(P) - 2)
    tc = P[i - 1] - P[i + 1]
    tc = tc / (np.linalg.norm(tc) or 1.0)
    return _fit_span(P[:i + 1], t1, tc, tol, depth + 1) + _fit_span(P[i:], -tc, t2, tol, depth + 1)


def _tangent(P, start, k=4):
    v = (P[min(k, len(P) - 1)] - P[0]) if start else (P[max(-k - 1, -len(P))] - P[-1])
    n = np.linalg.norm(v)
    return v / n if n else np.array([1.0, 0.0])


def curves(P, closed, tol):
    """Polyline -> smooth cubic segments: resample, find corners, smooth between, fit."""
    P = resample(np.asarray(P, float), closed)
    if len(P) < 4:
        return []
    cor = corner_mask(P, closed)
    P = smooth(P, closed, cor)
    if closed:
        if not cor.any():
            cor[0] = cor[len(P) // 2] = True
        s = int(np.argmax(cor))
        P, cor = np.roll(P, -s, axis=0), np.roll(cor, -s)
        P, cor = np.vstack([P, P[:1]]), np.append(cor, True)
    cor[0] = cor[-1] = True
    segs = []
    for a, b in zip(np.where(cor)[0][:-1], np.where(cor)[0][1:]):
        sp = P[a:b + 1]
        if len(sp) > 1:
            segs += _fit_span(sp, _tangent(sp, True), _tangent(sp, False), tol)
    return segs


# ------------------------------------------------------------------- output

def path_d(rings, close):
    out = []
    for segs in rings:
        if not segs:
            continue
        f = lambda v: f"{v[0]:.3f},{v[1]:.3f}"
        out.append("M" + f(segs[0][0]))
        for p1, c1, c2, p2 in segs:
            out.append(f"C{f(c1)} {f(c2)} {f(p2)}")
        if close:
            out.append("Z")
    return "".join(out)


def piece(name, az):
    mesh, src = load(name)
    right, view, up = basis(az)
    poly = silhouette(mesh, right, view, up)
    occ = Occluder(mesh, right, view, up)
    detail = (visible_runs(occluding(mesh, view), occ, poly.exterior)
              + visible_runs(creases(mesh, view), occ, poly.exterior))
    x0, y0, x1, y1 = poly.bounds
    flip = lambda a: np.column_stack([np.asarray(a)[:, 0] - x0, y1 - np.asarray(a)[:, 1]])
    rings = [flip(poly.exterior.coords)] + [flip(r.coords) for r in poly.interiors]
    out = [curves(r, True, TOL_OUT) for r in rings]
    det = [c for c in (curves(flip(r), False, TOL_LINE) for r in detail) if c]
    fitted = np.vstack([_bez(s, np.linspace(0, 1, 24, endpoint=False)) for s in out[0]])
    return dict(name=name, src=src, az=az, w=x1 - x0, h=y1 - y0,
                fill=path_d(out, True), lines=path_d(det, False),
                nseg=sum(len(r) for r in out), ndet=sum(len(d) for d in det), nrun=len(det),
                drift=shapely.hausdorff_distance(shapely.linearrings(fitted),
                                                 shapely.linearrings(rings[0]), densify=0.2))


def _svg(w, h, body, title, note, pad=0.0):
    note = note.replace("--", "-")            # a double hyphen is illegal inside an XML comment
    return (f'<?xml version="1.0" encoding="UTF-8"?>\n<!-- {note} -->\n'
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{w + 2 * pad:.3f}mm" '
            f'height="{h + 2 * pad:.3f}mm" viewBox="{-pad:.3f} {-pad:.3f} '
            f'{w + 2 * pad:.3f} {h + 2 * pad:.3f}"><title>{title}</title>{body}</svg>\n')


def _stroke(p):
    return (f'<g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round">'
            f'<path stroke-width="{W_OUT}" d="{p["fill"]}"/>'
            f'<path stroke-width="{W_LINE}" d="{p["lines"]}"/></g>')


def write(p):
    note = (f'{p["name"]} side elevation from {p["src"]}, azimuth {p["az"]:g} deg, '
            f'generated by pieces/side_svg.py; 1 unit = 1 mm (true scale)')
    paths = {
        f'{p["name"]}_side.svg': _svg(p["w"], p["h"], f'<path fill="currentColor" d="{p["fill"]}"/>',
                                      f'{p["name"]} — side view', note),
        f'{p["name"]}_side_trace.svg': _svg(p["w"], p["h"], _stroke(p),
                                            f'{p["name"]} — side view, line trace', note, PAD),
    }
    for f, s in paths.items():
        open(os.path.join(OUTDIR, f), "w").write(s)
    return list(paths)


def write_sheets(ps):
    w = sum(p["w"] for p in ps) + GAP * (len(ps) - 1)
    h = max(p["h"] for p in ps)
    out = []
    for suffix, body in (("", lambda p: f'<path fill="currentColor" d="{p["fill"]}"/>'),
                         ("_trace", _stroke)):
        g, x = [], 0.0
        for p in ps:
            g.append(f'<g transform="translate({x:.3f},{h - p["h"]:.3f})">{body(p)}</g>')
            x += p["w"] + GAP
        f = f"elements_side{suffix}.svg"
        open(os.path.join(OUTDIR, f), "w").write(_svg(
            w, h, "".join(g), "ORGANISM elements — side views",
            "EAT / MOVE / GROW side elevations at true relative scale, 1 unit = 1 mm",
            PAD if suffix else 0.0))
        out.append(f)
    return out


if __name__ == "__main__":
    args = sys.argv[1:]
    az = float(args.pop(0)) if args and args[0].lstrip("-").replace(".", "").isdigit() else 0.0
    names = [a.upper() for a in args] or ["EAT", "MOVE", "GROW"]
    os.makedirs(OUTDIR, exist_ok=True)
    ps = []
    for n in names:
        p = piece(n, az)
        ps.append(p)
        write(p)
        print(f'{p["name"]:5s} {p["w"]:6.2f} x {p["h"]:6.2f} mm | outline {p["nseg"]:3d} curves, '
              f'drift {p["drift"]*1000:4.1f} um | detail {p["nrun"]:2d} lines, {p["ndet"]:3d} curves')
    if len(ps) > 1:
        print("sheets:", ", ".join(write_sheets(ps)))
