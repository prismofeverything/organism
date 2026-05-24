"""FOOD - a stackable food token, built as ONE solid of revolution (no booleans, so the
mesh is watertight / printable). The whole part is a single meridian: the upward-flaring
body PLUS the universal connector (peg dome+ridge on top + matching socket below), revolved
once. The connector matches graft_connector.py's spec, so food still mates with the pieces
and with other food. Principles + pipeline: see FOOD.md and sor.py.

Run:  blender --background --python build_food.py
Tune via env:
  FOOD_R       shoulder radius (max Ø ~ 2R)
  FOOD_FLAREUP how far the top flares UP at the rim
  FOOD_WALL    injection-molding wall / floor thickness (invariant)
  FOOD_GAP     connector fit: + = clearance (slip), - = interference (snap)
  FOOD_OUT     output .obj path (also writes .stl + .profile.json next to it)
"""
import bpy, sys, os, json
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import graft_connector as gc          # connector dimensions (the universal spec)
import sor                            # the solids-of-revolution library

R        = float(os.environ.get("FOOD_R",       "13.0"))
FLARE_UP = float(os.environ.get("FOOD_FLAREUP",  "2.5"))
WALL     = float(os.environ.get("FOOD_WALL",     "2.5"))
GAP      = float(os.environ.get("FOOD_GAP",      "0.15"))   # + slip / - snap
OUT      = os.environ.get("FOOD_OUT",
             os.path.join(os.path.dirname(os.path.abspath(__file__)), "renders/food/FOOD_new.obj"))

# the whole part as one meridian (body + peg + socket); connector dims from the universal spec
prof, meta = sor.food(R=R, flare_up=FLARE_UP, wall=WALL, gap=GAP,
                      dome_r=gc.DOME_DIA / 2, dome_h=gc.DOME_HEIGHT,
                      ridge_ir=gc.RIDGE_ID / 2, ridge_or=gc.RIDGE_OD / 2,
                      ridge_h=gc.RIDGE_HEIGHT, peak_w=gc.RIDGE_PEAK_W)

# continuity invariant on the VISIBLE top flare (the connector + socket have intentional edges)
top = sor.Meridian(); top.pts = [p for p in prof if p[0] >= meta["ridge_or"] - 1e-6 and p[1] >= meta["floor_z"] - 0.1]
he = top.hard_edges()
print("CONTINUITY (top flare): " + ("G1 OK (no tangent break > 12°)" if not he else "%d hard edge(s)" % len(he)))

# one revolve -> one watertight surface (no booleans to shatter it)
bpy.ops.wm.read_factory_settings(use_empty=True)
body = sor.revolve(prof, "FOOD", seg=120)
nonman = sor.manifold_report(body)
sor.export(body, os.path.splitext(OUT)[0])

with open(os.path.splitext(OUT)[0] + ".profile.json", "w") as f:
    json.dump({"prof": prof, "wall": WALL, **meta}, f)

zr = [v.co.z for v in body.data.vertices]
print("FOOD: maxØ=%.1f, height=%.1f mm, wall=%.1f, gap=%.2f | non-manifold edges: %d (0 = watertight/printable)"
      % (2 * max(p[0] for p in prof), max(zr) - min(zr), WALL, GAP, nonman))
print("wrote", OUT, "(+.stl, +.profile.json)")
