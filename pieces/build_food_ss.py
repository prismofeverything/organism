"""FOOD self-supporting variant: pointed dome cap + tented ridge roof (sor._socket_ss) so the
SOCKET prints support-free when oriented socket-DOWN, WITHOUT touching the mating flanks (the grip
is unchanged - see the fit check in the review). Builds BOTH fits to *_ss.{obj,stl,profile.json};
leaves the originals (FOOD_snap / FOOD_slip) intact.

Run:  blender --background --python build_food_ss.py
Env:  FOOD_SS_DEG (ceiling angle from vertical, default 40), FOOD_SS_CLR (roof clearance, default 0.3)
"""
import bpy, sys, os, json
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import graft_connector as gc          # the universal peg spec (unchanged - only the food socket changes)
import sor

R, FLARE_UP, WALL = 13.0, 2.5, 2.5
SS_DEG = float(os.environ.get("FOOD_SS_DEG", "30.0"))   # 30deg from vertical: clean at PrusaSlicer default auto-support
SS_CLR = float(os.environ.get("FOOD_SS_CLR", "0.3"))
FOOD = os.path.join(os.path.dirname(os.path.abspath(__file__)), "renders", "food")

kw = dict(R=R, flare_up=FLARE_UP, wall=WALL,
          dome_r=gc.DOME_DIA / 2, dome_h=gc.DOME_HEIGHT,
          ridge_ir=gc.RIDGE_ID / 2, ridge_or=gc.RIDGE_OD / 2,
          ridge_h=gc.RIDGE_HEIGHT, peak_w=gc.RIDGE_PEAK_W,
          self_support=True, ss_deg=SS_DEG, ss_clr=SS_CLR)

# gap: + = clearance/slip, - = interference/snap (matches build_food.py's built pair)
for name, gap in [("FOOD_snap_ss", -0.10), ("FOOD_slip_ss", 0.15)]:
    prof, meta = sor.food(gap=gap, **kw)
    bpy.ops.wm.read_factory_settings(use_empty=True)
    body = sor.revolve(prof, name, seg=120)
    nonman = sor.manifold_report(body)
    out = os.path.join(FOOD, name)
    sor.export(body, out)
    with open(out + ".profile.json", "w") as f:
        json.dump({"prof": prof, "wall": WALL, **meta}, f)
    zr = [v.co.z for v in body.data.vertices]
    print("%-14s maxO=%.1f height=%.2f gap=%+.2f ss=%.0fdeg nonman=%d (0=watertight) apex=%.2f -> %s"
          % (name, 2 * max(p[0] for p in prof), max(zr) - min(zr), gap, SS_DEG, nonman,
             meta["socket_ceiling"], out + ".stl"))
print("done - originals FOOD_snap/FOOD_slip left intact")
