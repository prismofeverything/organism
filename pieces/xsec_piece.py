"""Cross-section a grafted piece (up=Y) through its central axis to inspect the connector
graft / top continuity. Light: trimesh + matplotlib, no Blender.
Usage: python xsec_piece.py EAT_connected.obj [other.obj ...]"""
import sys, os, numpy as np, trimesh, matplotlib
matplotlib.use("Agg"); import matplotlib.pyplot as plt
files = sys.argv[1:] or ["EAT_connected.obj"]
fig, axs = plt.subplots(len(files), 2, figsize=(11, 6*len(files)), squeeze=False)
for i, f in enumerate(files):
    m = trimesh.load(f, process=False, force='mesh')
    ymax = float(m.bounds[1][1])
    a0, a1 = axs[i][0], axs[i][1]
    for normal, hx, c, lab in [([0,0,1], 0, 'C0', 'slice ⊥Z'), ([1,0,0], 2, 'C1', 'slice ⊥X')]:
        sec = m.section(plane_origin=[0,0,0], plane_normal=normal)
        if sec is None: continue
        for poly in sec.discrete:
            a0.plot(poly[:,hx], poly[:,1], '-', lw=0.5, color=c)
            a1.plot(poly[:,hx], poly[:,1], '-', lw=1.1, color=c, label=lab)
    a0.set_title(os.path.basename(f) + "  — full profile")
    a1.set_title("top: connector graft (sharp corner = hard bisect)")
    a1.set_xlim(-11, 11); a1.set_ylim(ymax-20, ymax+2)
    for a in (a0, a1):
        a.set_aspect('equal'); a.grid(alpha=0.3); a.axhline(ymax, color='r', ls=':', lw=0.7)
    hh, ll = a1.get_legend_handles_labels(); uniq = dict(zip(ll, hh))
    a1.legend(uniq.values(), uniq.keys(), fontsize=8)
out = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                   "renders/xsec_" + "_".join(os.path.basename(f).replace('.obj','') for f in files) + ".png")
plt.tight_layout(); plt.savefig(out, dpi=120); print("wrote", out)
