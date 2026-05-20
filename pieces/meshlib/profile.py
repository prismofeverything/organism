"""
Side-profile transfer functions: normalized field u in [0,1] -> top-surface
height. Derived so that on a disc (where u = 1 - (r/a)^2) the radius-vs-height
profile matches the classic curves exactly:

    parabola:    r = a*sqrt(1-t)        -> f(u) = u
    hemisphere:  r = a*sqrt(1-t^2)      -> f(u) = sqrt(u)
    shallow:     r = a*(1-t)^(1/4)      -> f(u) = 1 - (1-u)^2
    cosine:      r = a*cos(t*pi/2)      -> f(u) = (2/pi)*arccos(sqrt(1-u))

`wall_frac` raises the whole top surface off a vertical side wall of that
fraction of z_max (the old `cyl_top`): the solid is a full prism up to the wall,
then the dome above. Boundary height = wall, center height = z_max.
"""
from __future__ import annotations
import numpy as np

SHAPES = ("parabola", "hemisphere", "shallow", "cosine")


def transfer(u, kind="parabola"):
    u = np.clip(u, 0.0, 1.0)
    if kind == "parabola":   return u
    if kind == "hemisphere": return np.sqrt(u)
    if kind == "shallow":    return 1.0 - (1.0 - u) ** 2
    if kind == "cosine":     return (2 / np.pi) * np.arccos(np.sqrt(1.0 - u))
    raise ValueError(f"unknown shape {kind!r}")


def height(u, z_max, kind="parabola", wall_frac=0.0):
    wall = wall_frac * z_max
    return wall + (z_max - wall) * transfer(u, kind)


def _smoothmin(a, b, k):
    """Smooth minimum (k larger -> sharper). Handles +inf operands."""
    m = np.minimum(a, b)
    return m - np.log1p(np.exp(-k * np.abs(a - b))) / k


def height_field(u, d, z_max, kind="parabola", wall_frac=0.0,
                 fillet=4.0, round_k=1.5):
    """Top-surface height z = H(x,y) for the closed solid.

    u : normalized membrane field in [0,1] (0 on boundary, 1 at the crest)
    d : distance to the boundary (mm), >= 0

    Construction: a vertical wall of height W = wall_frac*z_max, a quarter-round
    FILLET of radius rf rising from the wall (tangent to it, so the shoulder is
    smooth), soft-min-blended into the membrane DOME (smooth, no medial creases).
    The fillet caps how fast the surface can climb near the rim, which is what
    removes the sharp wall->dome shoulder.
    """
    W = wall_frac * z_max
    dome_h = z_max - W
    dome = dome_h * transfer(u, kind)                      # 0 .. dome_h
    rf = min(fillet, max(dome_h, 1e-6))
    arc = np.where(d < rf, np.sqrt(np.clip(rf * rf - (rf - d) ** 2, 0, None)),
                   np.inf)                                 # rounded cap near rim
    return W + _smoothmin(dome, arc, round_k)
