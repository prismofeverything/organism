"""
Explain the height-field / membrane-field construction with pictures.

Produces two figures:
  fig1_crest.png  - silhouette is exact; crest is a POINT for a disc and a
                    3-fold SPIRAL CURVE for the spiral; surface is smooth.
  fig2_oldnew.png - what the OLD radial-scaling method lost (gap fill) vs the
                    NEW height field (nothing); plus a smooth parabolic
                    cross-section across an arm.

This also doubles as a first proof that solving the elastic-membrane equation
    laplacian(u) = -1   inside R,   u = 0   on the boundary
gives the right shape: a paraboloid over a disc, a winding ridge over a spiral.

Pure numpy/scipy/matplotlib (the standalone stack), no Blender.
"""
import numpy as np
import scipy.sparse as sp
import scipy.sparse.linalg as spla
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from pathlib import Path

OUT = Path(__file__).parent
N = 221                         # grid resolution
xs = np.linspace(-1, 1, N)
X, Y = np.meshgrid(xs, xs)
H = xs[1] - xs[0]              # grid spacing


# ---------------------------------------------------------------- regions
def disc_mask(radius=0.8):
    return (X**2 + Y**2) <= radius**2


def spiral_spine(n=500):
    """Centerline of ONE arm (then replicated 3x). Returns (pts, widths).
    A spiral that wraps ~235 deg so its tip curls back inward -> NON-star."""
    t = np.linspace(0, 1, n)
    r0, r1 = 0.15, 0.86
    a0 = np.pi / 2
    wrap = np.radians(235)
    hook = np.radians(55) * np.clip((t - 0.8) / 0.2, 0, 1) ** 2   # tip curl
    rr = r0 + (r1 - r0) * t
    ang = a0 + wrap * t + hook
    w = 0.20 + (0.05 - 0.20) * t                                  # taper
    return np.c_[rr * np.cos(ang), rr * np.sin(ang)], w


def spiral_mask():
    """Union of 3 arms (120 deg apart) + a central hub. Also return the full
    3-fold spine point set for drawing the crest."""
    pts, w = spiral_spine()
    # central hub so the three arms join in the middle
    hub = np.array([[0.0, 0.0]])
    mask = np.zeros_like(X, dtype=bool)
    spine_all = []
    for k in range(3):
        a = k * 2 * np.pi / 3
        R = np.array([[np.cos(a), -np.sin(a)], [np.sin(a), np.cos(a)]])
        rp = pts @ R.T
        spine_all.append(rp)
        for (cx, cy), ww in zip(rp, w):
            mask |= (X - cx) ** 2 + (Y - cy) ** 2 <= (ww / 2) ** 2
        # hub
        mask |= (X - hub[0, 0]) ** 2 + (Y - hub[0, 1]) ** 2 <= 0.19 ** 2
    return mask, np.vstack(spine_all)


# ---------------------------------------------------------------- membrane
def membrane(mask):
    """Solve laplacian(u) = -1 inside mask, u = 0 on boundary. Returns u in
    [0,1] with NaN outside the region (for clean plotting)."""
    idx = -np.ones(mask.shape, dtype=int)
    inside = np.argwhere(mask)
    idx[mask] = np.arange(len(inside))
    M = len(inside)
    rows, cols, vals = [], [], []
    b = np.full(M, H * H)
    for p, (i, j) in enumerate(inside):
        rows.append(p); cols.append(p); vals.append(4.0)
        for di, dj in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            ni, nj = i + di, j + dj
            if 0 <= ni < mask.shape[0] and 0 <= nj < mask.shape[1] and mask[ni, nj]:
                rows.append(p); cols.append(idx[ni, nj]); vals.append(-1.0)
            # else: Dirichlet u=0 -> contributes nothing
    A = sp.csr_matrix((vals, (rows, cols)), shape=(M, M))
    u = spla.spsolve(A, b)
    u = u / u.max()
    grid = np.full(mask.shape, np.nan)
    grid[mask] = u
    return grid


def profile(u, kind="parabola"):
    """Side-profile transfer function applied to normalized field u in [0,1]."""
    v = np.clip(u, 0, 1)
    if kind == "parabola":   return v
    if kind == "hemisphere": return np.sqrt(np.clip(1 - (1 - v) ** 2, 0, 1))
    if kind == "shallow":    return v ** 0.5
    return v


# ================================================================ FIGURE 1
disc = disc_mask()
u_disc = membrane(disc)
spi, spine = spiral_mask()
u_spi = membrane(spi)

ext = [-1, 1, -1, 1]
fig = plt.figure(figsize=(13, 8.4))
fig.suptitle("Height field over the SVG region:  shadow is EXACT;  the crest is "
             "the shape's own spine", fontsize=14, weight="bold")

rows = [("Disc  (round  ->  EAT / GROW)", disc, u_disc, None),
        ("Spiral  (->  MOVE)", spi, u_spi, spine)]

for r, (label, mask, u, crest) in enumerate(rows):
    # col A: top-down shadow
    ax = fig.add_subplot(2, 3, r * 3 + 1)
    ax.imshow(np.where(mask, 1.0, np.nan), extent=ext, origin="lower",
              cmap="Greys", vmin=0, vmax=1.6)
    ax.set_title("(A) top-down shadow = region\n(EXACT — 0 silhouette loss)", fontsize=10)
    ax.set_ylabel(label, fontsize=11, weight="bold")
    ax.set_xticks([]); ax.set_yticks([])

    # col B: level sets + crest
    ax = fig.add_subplot(2, 3, r * 3 + 2)
    ax.contourf(X, Y, np.nan_to_num(u, nan=0.0), levels=18, cmap="viridis")
    ax.contour(X, Y, np.nan_to_num(u, nan=0.0), levels=10, colors="white",
               linewidths=0.5, alpha=0.7)
    if crest is None:
        ax.plot(0, 0, "r*", ms=18)
        ax.set_title("(B) crest = a single POINT", fontsize=10)
    else:
        ax.plot(crest[:, 0], crest[:, 1], "r.", ms=2)
        ax.set_title("(B) crest = the spine: a 3-fold SPIRAL CURVE\n"
                     "(continuous, 120deg-symmetric, NOT a circle)", fontsize=10)
    ax.set_xlim(-1, 1); ax.set_ylim(-1, 1); ax.set_aspect("equal")
    ax.set_xticks([]); ax.set_yticks([])

    # col C: 3D surface
    ax = fig.add_subplot(2, 3, r * 3 + 3, projection="3d")
    Z = profile(u, "parabola")
    ax.plot_surface(X, Y, np.nan_to_num(Z, nan=np.nan), cmap="viridis",
                    rstride=2, cstride=2, linewidth=0, antialiased=True)
    ax.set_title("(C) smooth surface,  0 at the rim\n(continuous — no creases)", fontsize=10)
    ax.set_zlim(0, 1.05); ax.set_xticks([]); ax.set_yticks([]); ax.set_zticks([])
    ax.view_init(elev=42, azim=-58)

fig.tight_layout(rect=[0, 0, 1, 0.95])
fig.savefig(OUT / "fig1_crest.png", dpi=130)
print("wrote", OUT / "fig1_crest.png")


# ================================================================ FIGURE 2
def radial_hull(mask, nbins=720):
    """The OLD method's silhouette: union of the outline scaled toward the
    origin == fill every ray out to the region's farthest point at that angle.
    For a non-star spiral this floods the gaps."""
    yy, xx = np.nonzero(mask)
    ang = np.arctan2(Y[yy, xx], X[yy, xx])
    rad = np.hypot(X[yy, xx], Y[yy, xx])
    bins = ((ang + np.pi) / (2 * np.pi) * nbins).astype(int) % nbins
    rmax = np.zeros(nbins)
    np.maximum.at(rmax, bins, rad)
    # light angular smoothing
    k = np.ones(9) / 9
    rmax = np.convolve(np.r_[rmax[-4:], rmax, rmax[:4]], k, "same")[4:-4]
    A = np.arctan2(Y, X); Rg = np.hypot(X, Y)
    b = ((A + np.pi) / (2 * np.pi) * nbins).astype(int) % nbins
    return Rg <= rmax[b]


old = radial_hull(spi)

fig = plt.figure(figsize=(13, 4.6))
fig.suptitle("Why the old method lost the silhouette — and the new one doesn't",
             fontsize=14, weight="bold")

ax = fig.add_subplot(1, 3, 1)
ax.imshow(np.where(spi, 1.0, np.nan), extent=ext, origin="lower", cmap="Greens", vmin=0, vmax=1.4)
ax.set_title("(A) NEW: height-field shadow\n= SVG spiral (exact)", fontsize=10)
ax.set_xticks([]); ax.set_yticks([])

ax = fig.add_subplot(1, 3, 2)
ax.imshow(np.where(old & ~spi, 1.0, np.nan), extent=ext, origin="lower", cmap="Reds", vmin=0, vmax=1.6)
ax.imshow(np.where(spi, 1.0, np.nan), extent=ext, origin="lower", cmap="Greens", vmin=0, vmax=1.4)
ax.set_title("(B) OLD: taper-to-a-point shadow\nRED = gaps flooded (silhouette lost)", fontsize=10)
ax.set_xticks([]); ax.set_yticks([])

# cross-section across an arm
ax = fig.add_subplot(1, 3, 3)
row = int(N * 0.74)                     # a horizontal line crossing an upper arm
line = u_spi[row, :]
ax.plot(xs, np.nan_to_num(line, nan=0.0), lw=2.2, color="purple")
ax.fill_between(xs, 0, np.nan_to_num(line, nan=0.0), alpha=0.15, color="purple")
ax.set_title("(C) cross-section across an arm\n= smooth parabola, 0 at both edges", fontsize=10)
ax.set_ylim(0, 1.05); ax.set_xlabel("across the arm"); ax.set_ylabel("height")
ax.grid(alpha=0.3)

fig.tight_layout(rect=[0, 0, 1, 0.92])
fig.savefig(OUT / "fig2_oldnew.png", dpi=130)
print("wrote", OUT / "fig2_oldnew.png")
