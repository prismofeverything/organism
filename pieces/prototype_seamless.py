"""Prototype the SEAMLESS connector crown as a continuous meridian (sor.py) and overlay it
on the current flat-shelf graft cross-section -- shows the proposed top profile BEFORE any
Blender graft. Light: trimesh + matplotlib.
Usage: python prototype_seamless.py [EAT_connected.obj] [blend_below_apex_mm] [crown_rise_mm]"""
import sys, os, numpy as np, trimesh, matplotlib
matplotlib.use("Agg"); import matplotlib.pyplot as plt
HERE = os.path.dirname(os.path.abspath(__file__)); sys.path.insert(0, HERE)
import sor

src         = sys.argv[1] if len(sys.argv) > 1 else "EAT_connected.obj"
BLEND_BELOW = float(sys.argv[2]) if len(sys.argv) > 2 else 5.0
CROWN_RISE  = float(sys.argv[3]) if len(sys.argv) > 3 else 2.5

m = trimesh.load(src, process=False, force='mesh')
ymax = float(m.bounds[1][1])
sec = m.section(plane_origin=[0,0,0], plane_normal=[0,0,1])      # vertical slice through axis
cur = [poly[:, [0,1]] for poly in sec.discrete]

co = m.vertices; yy = co[:,1]; rr = np.hypot(co[:,0], co[:,2])   # radius envelope per height
nb = 200; edges = np.linspace(yy.min(), ymax, nb); mid = 0.5*(edges[:-1]+edges[1:])
env = np.array([rr[(yy>=edges[i])&(yy<edges[i+1])].max() if np.any((yy>=edges[i])&(yy<edges[i+1])) else np.nan
                for i in range(nb-1)])
env_at = lambda y: float(env[np.nanargmin(np.abs(mid - y))])

dr, dh = 3.8/2, 4.3                                             # peg spec (mirrors graft_connector.py
ir, orr, rh, pw = 8.3/2, 12.8/2, 2.75, 2.0                      # & sor.food defaults; kept exact for the mate)

y_blend = ymax - BLEND_BELOW
R_blend = env_at(y_blend); R_lo = env_at(y_blend - 2.0)
m0 = 2.0 / ((R_blend - R_lo) or -1e-6)                           # body wall slope dz/dr at blend
floor_z = y_blend + CROWN_RISE

md = sor.Meridian(R_blend, y_blend)
md.hermite_to(orr, floor_z, m0=m0, m1=0.0, n=40)                 # tangent crown: body -> ridge base
crown = md.pts
prop = crown + sor._ridge(ir, orr, rh, pw, floor_z)[::-1][1:] + [(dr, floor_z)] + sor._dome(dr, dh, floor_z)[1:]
P = np.array(prop)

he = sor.Meridian(); he.pts = crown; breaks = he.hard_edges(15.0)
print("crown G1: %s | R_blend=%.1f@z%.1f -> ridge base z%.1f | body dz/dr=%.2f | peg tip z%.1f"
      % ("clean" if not breaks else "%d break(s)" % len(breaks), R_blend, y_blend, floor_z, m0, P[:,1].max()))

fig, ax = plt.subplots(figsize=(8.5, 8))
for i, s in enumerate(cur):
    ax.plot(s[:,0], s[:,1], '-', color='0.65', lw=1.1, label='current (flat shelf)' if i == 0 else None)
ax.plot(P[:,0], P[:,1], '-', color='C3', lw=2.3, label='proposed seamless crown')
ax.plot(-P[:,0], P[:,1], '-', color='C3', lw=2.3)
ax.axhline(floor_z, color='C0', ls=':', lw=.7); ax.text(7.5, floor_z+.15, 'ridge base', color='C0', fontsize=8)
ax.set_aspect('equal'); ax.grid(alpha=.3); ax.set_xlim(-11, 11); ax.set_ylim(ymax-15, ymax+3)
ax.legend(fontsize=9, loc='lower center')
ax.set_title(os.path.basename(src) + ": current flat-shelf vs proposed seamless crown")
out = os.path.join(HERE, "renders/seamless_" + os.path.basename(src).replace('.obj','') + ".png")
plt.tight_layout(); plt.savefig(out, dpi=120); print("wrote", out)
