"""
The universal connector as a single-valued radial HEIGHT FIELD — the core of the
generative graft (GRAFTING.md approach F, principle-7 generalized). The peg is a
solid of revolution, so every planar radius d maps to ONE height; that makes it a
boss we can add to the body's height field z=H(x,y) instead of booleaning a mesh
onto a sculpt. Dims mirror graft_connector.py — keep in sync.

Top surface, height above the seat plane:
    d in [0, DOME_R]          parabolic dome, apex DOME_HEIGHT at d=0
    d in [DOME_R, RIDGE_IR]   flat valley at the seat (z=0)
    d in [RIDGE_IR, RIDGE_OR] ridge bump, peak RIDGE_HEIGHT
    d in [RIDGE_OR, R_SEAT]   flat seat margin (z=0)
    d  >  R_SEAT              body — a COLLAR sweeps down to the star wall, doing
                             the star->circle morph so the seat is a clean circle.
"""
from __future__ import annotations
import numpy as np

# --- connector spec (mirror graft_connector.py) ---
DOME_HEIGHT  = 4.3
DOME_R       = 3.8 / 2          # 1.90
RIDGE_OD     = 12.8
RIDGE_ID     = 8.3
RIDGE_OR     = RIDGE_OD / 2     # 6.40
RIDGE_IR     = RIDGE_ID / 2     # 4.15
RIDGE_HEIGHT = 2.75
RIDGE_PEAK_W = 2.0
SEAT_MARGIN  = 0.6
R_SEAT       = RIDGE_OR + SEAT_MARGIN     # 7.00  functional radius; connector exact within


def seat_z(zmax):
    """Height of the seat plane: the connector dome apex sits at zmax."""
    return zmax - DOME_HEIGHT


def peg_height(d):
    """c(d): connector surface height ABOVE the seat plane, single-valued in d.
    Zero for d >= RIDGE_OR (the flat seat). Vectorized."""
    d = np.asarray(d, float)
    z = np.zeros_like(d)
    # parabolic dome
    m = d <= DOME_R
    z[m] = DOME_HEIGHT * (1.0 - (d[m] / DOME_R) ** 2)
    # ridge bump (mirror of build_ridge_ring's quadratic-shouldered profile)
    m = (d >= RIDGE_IR) & (d <= RIDGE_OR)
    u = (d[m] - RIDGE_IR) / (RIDGE_OR - RIDGE_IR)      # 0 inner .. 1 outer
    uc = np.abs(2 * u - 1)                             # 0 center .. 1 edges
    cf = RIDGE_PEAK_W / (RIDGE_OD - RIDGE_ID)          # central flat fraction
    ef = max(1.0 - cf, 1e-6)
    bump = np.where(uc < cf, 1.0, 1.0 - ((uc - cf) / ef) ** 2)
    z[m] = RIDGE_HEIGHT * bump
    return z


SOCKET_GAP = 0.20              # CLEARANCE 0.15 + IM tolerance 0.05 (mirror graft_connector)
DOME_R_S  = DOME_R + SOCKET_GAP        # 2.10
DOME_H_S  = DOME_HEIGHT + SOCKET_GAP   # 4.50
RIDGE_IR_S = RIDGE_IR - SOCKET_GAP     # 3.95
RIDGE_OR_S = RIDGE_OR + SOCKET_GAP     # 6.60
RIDGE_H_S = RIDGE_HEIGHT + SOCKET_GAP  # 2.95
R_SOCK    = RIDGE_OR_S                  # socket mouth radius


def socket_height(d):
    """Cavity-ceiling height ABOVE the bottom plane for the SOCKET (the receiving
    end): the oversized mirror of the peg, single-valued in d. Zero outside the
    ridge (flat bottom / seat). The food/piece peg below seats into this cavity."""
    d = np.asarray(d, float)
    z = np.zeros_like(d)
    m = d <= DOME_R_S
    z[m] = DOME_H_S * (1.0 - (d[m] / DOME_R_S) ** 2)          # dome cavity
    m = (d >= RIDGE_IR_S) & (d <= RIDGE_OR_S)                 # ridge groove
    u = (d[m] - RIDGE_IR_S) / (RIDGE_OR_S - RIDGE_IR_S)
    uc = np.abs(2 * u - 1)
    cf = RIDGE_PEAK_W / (2 * (RIDGE_OR_S - RIDGE_IR_S))
    ef = max(1.0 - cf, 1e-6)
    z[m] = RIDGE_H_S * np.where(uc < cf, 1.0, 1.0 - ((uc - cf) / ef) ** 2)
    return z


def _bezier(t, P0, P1, P2, P3):
    t = t[:, None]
    return ((1 - t) ** 3 * P0 + 3 * (1 - t) ** 2 * t * P1
            + 3 * (1 - t) * t ** 2 * P2 + t ** 3 * P3)


def collar_meridian(R0, floor_z, knee_frac=0.75, fullness=0.6, n=60):
    """(r, z) meridian of the COLLAR for a ray whose rim radius is R0: a G1 cubic
    from the circular seat ring (R_SEAT, floor_z) — leaving HORIZONTAL — down to the
    top of the vertical wall (R0, z_knee) — arriving VERTICAL. Because every ray
    ends at the same seat radius R_SEAT but starts at its own R0, the collar is
    longer in star-point directions and the cross-sections morph star->circle as
    they rise. Returns points ordered seat -> wall."""
    z_knee = knee_frac * floor_z
    P0 = np.array([R_SEAT, floor_z])                  # seat ring (top, inner)
    P3 = np.array([R0, z_knee])                       # wall top (outer)
    P1 = P0 + np.array([fullness * (R0 - R_SEAT), 0.0])       # horizontal tangent at seat
    P2 = P3 + np.array([0.0, fullness * (floor_z - z_knee)])  # vertical tangent at wall
    t = np.linspace(0, 1, n)
    return _bezier(t, P0, P1, P2, P3)


def full_meridian(R0, zmax, knee_frac=0.75, fullness=0.6):
    """Full piece meridian along one ray (rim radius R0), center -> rim:
    connector (dome+groove+ridge+seat) + collar + vertical wall to z=0.
    Returns (r, z) Nx2, ordered from the axis outward/down."""
    fz = seat_z(zmax)
    # connector + seat: r in [0, R_SEAT], z = fz + peg_height(r)
    rc = np.linspace(0, R_SEAT, 80)
    seat = np.column_stack([rc, fz + peg_height(rc)])
    collar = collar_meridian(R0, fz, knee_frac, fullness)
    z_knee = knee_frac * fz
    wall = np.array([[R0, z_knee], [R0, 0.0]])
    return np.vstack([seat, collar, wall])
