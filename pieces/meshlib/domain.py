"""
Stage 1 — domain: SVG silhouette -> centered 2D region R.

A region is one outer boundary polyline plus zero or more hole polylines
(both as closed polylines, CCW outer / CW holes by convention). Coordinates
are converted from SVG (y-down) to math (y-up), recentered on the outer
boundary's centroid-of-area, and scaled to the canonical footprint.
"""
from __future__ import annotations
import re
from dataclasses import dataclass, field
from pathlib import Path as FsPath
import numpy as np
from svgelements import Path as SvgPath

FOOTPRINT_MM = 37.0


@dataclass
class Region2D:
    name: str
    outer: np.ndarray                       # (N,2) closed polyline, CCW
    holes: list = field(default_factory=list)  # list of (M,2) polylines, CW

    @property
    def all_rings(self):
        return [self.outer, *self.holes]

    def signed_area(self, ring):
        x, y = ring[:, 0], ring[:, 1]
        return 0.5 * np.sum(x * np.roll(y, -1) - np.roll(x, -1) * y)

    def area(self):
        return abs(self.signed_area(self.outer)) - sum(
            abs(self.signed_area(h)) for h in self.holes)

    def bounds(self):
        p = self.outer
        return p[:, 0].min(), p[:, 1].min(), p[:, 0].max(), p[:, 1].max()


def _first_path_d(svg_text: str) -> str:
    m = re.search(r'<path[^>]*\sd="([^"]+)"', svg_text)
    if not m:
        raise ValueError("no <path d=...> found in SVG")
    return m.group(1)


def _subpaths(d: str):
    """Yield lists of (x,y) per subpath, sampling curves by arc length."""
    path = SvgPath(d)
    try:
        subs = list(path.as_subpaths())
    except AttributeError:                   # older svgelements
        subs = [path]
    for sub in subs:
        sub = SvgPath(*sub) if not isinstance(sub, SvgPath) else sub
        try:
            L = sub.length()
        except TypeError:
            L = sub.length(error=1e-4)
        n = max(16, int(np.ceil(L)))         # ~1 SVG-unit spacing
        pts = []
        for i in range(n):
            p = sub.point(i / n)
            pts.append((float(p.x), float(p.y)))
        if len(pts) >= 3:
            yield pts


def _shoelace(poly):
    x = poly[:, 0]; y = poly[:, 1]
    return 0.5 * np.sum(x * np.roll(y, -1) - np.roll(x, -1) * y)


def _centroid_of_area(poly):
    x = poly[:, 0]; y = poly[:, 1]
    cross = x * np.roll(y, -1) - np.roll(x, -1) * y
    A = 0.5 * np.sum(cross)
    cx = np.sum((x + np.roll(x, -1)) * cross) / (6 * A)
    cy = np.sum((y + np.roll(y, -1)) * cross) / (6 * A)
    return np.array([cx, cy])


def distance_to_boundary(pts, region) -> np.ndarray:
    """Per-point distance to the region boundary (outer + holes)."""
    pts = np.asarray(pts, float)
    best = np.full(len(pts), np.inf)
    for ring in region.all_rings:
        a = ring
        ab = np.roll(ring, -1, axis=0) - a
        L2 = np.einsum("ij,ij->i", ab, ab) + 1e-12
        for i in range(len(a)):
            ap = pts - a[i]
            t = np.clip((ap @ ab[i]) / L2[i], 0, 1)
            proj = a[i] + ab[i] * t[:, None]
            best = np.minimum(best, np.hypot(*(pts - proj).T))
    return best


def round_corners(region, radius=1.0, spacing=0.3) -> Region2D:
    """Fillet sharp silhouette corners (convex tips AND concave notches) by ~`radius`
    mm via curvature-flow smoothing of the outline. Sharp corners can't be smooth
    surfaces and make sliver triangles / fragile mold features; a small round fixes
    both with a sub-1% silhouette change. Straight/gently-curved spans barely move."""
    iters = max(1, int(round((radius / spacing) ** 2)))

    def _round(ring):
        loop = np.vstack([ring, ring[:1]])
        seg = np.hypot(*np.diff(loop, axis=0).T)
        s = np.concatenate([[0], np.cumsum(seg)]); total = s[-1]
        n = max(16, int(total / spacing))
        si = np.linspace(0, total, n, endpoint=False)
        p = np.column_stack([np.interp(si, s, loop[:, 0]),
                             np.interp(si, s, loop[:, 1])])
        for _ in range(iters):
            p += 0.5 * ((np.roll(p, 1, 0) + np.roll(p, -1, 0)) / 2 - p)
        return p

    return Region2D(region.name, _round(region.outer),
                    [_round(h) for h in region.holes])


def load_region(svg_path, name=None, footprint=FOOTPRINT_MM) -> Region2D:
    svg_path = FsPath(svg_path)
    name = name or svg_path.stem.upper()
    d = _first_path_d(svg_path.read_text())

    rings = []
    for pts in _subpaths(d):
        arr = np.asarray(pts, float)
        arr[:, 1] = -arr[:, 1]               # SVG y-down -> math y-up
        # drop duplicate consecutive points
        keep = np.r_[True, np.any(np.diff(arr, axis=0) != 0, axis=1)]
        rings.append(arr[keep])

    # largest |area| ring is the outer boundary; the rest are holes
    rings.sort(key=lambda r: abs(_shoelace(r)), reverse=True)
    outer, holes = rings[0], rings[1:]

    # orient: outer CCW (area>0), holes CW (area<0)
    if _shoelace(outer) < 0:
        outer = outer[::-1]
    holes = [(h[::-1] if _shoelace(h) > 0 else h) for h in holes]

    # recenter on outer centroid-of-area, scale to footprint
    c = _centroid_of_area(outer)
    outer = outer - c
    holes = [h - c for h in holes]
    rmax = np.max(np.hypot(outer[:, 0], outer[:, 1]))
    s = (footprint / 2) / rmax
    outer *= s
    holes = [h * s for h in holes]
    return Region2D(name, outer, holes)


# --------------------------------------------------------------- diagnostics
if __name__ == "__main__":
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    here = FsPath(__file__).resolve().parent.parent
    out = here / "diagrams"
    fig, axes = plt.subplots(1, 3, figsize=(13, 4.6))
    for ax, fn in zip(axes, ["eat.svg", "move.svg", "grow.svg"]):
        reg = load_region(here / "inputs" / fn)
        ax.fill(reg.outer[:, 0], reg.outer[:, 1], alpha=0.3, color="steelblue")
        ax.plot(np.r_[reg.outer[:, 0], reg.outer[0, 0]],
                np.r_[reg.outer[:, 1], reg.outer[0, 1]], "-", lw=1.2, color="navy")
        for h in reg.holes:
            ax.fill(h[:, 0], h[:, 1], color="white")
            ax.plot(np.r_[h[:, 0], h[0, 0]], np.r_[h[:, 1], h[0, 1]], "r-", lw=1)
        ax.plot(0, 0, "k+", ms=10)
        x0, y0, x1, y1 = reg.bounds()
        ax.set_title(f"{reg.name}\n{len(reg.outer)} bdry pts, {len(reg.holes)} holes, "
                     f"area={reg.area():.0f}mm^2", fontsize=9)
        ax.set_aspect("equal"); ax.grid(alpha=0.3)
    fig.suptitle("Stage 1 (domain): parsed silhouettes, centered + scaled to 37mm",
                 fontsize=13, weight="bold")
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    p = out / "stage1_domains.png"
    fig.savefig(p, dpi=130)
    print("wrote", p)
