"""Production plate: 15x FOOD at gap +0.10 (the calibrated best fit from the ladder), self-supporting
socket (ss_deg=30), plain (no labels), 5x3 grid. Run: blender --background --python build_food10_plate.py
Env: FOOD10_GAP (default 0.10), PLATE_CELL (default 36), FOOD10_COLS/ROWS (default 5/3)."""
import bpy, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import graft_connector as gc
import sor

R, FLARE_UP, WALL = 13.0, 2.5, 2.5
GAP  = float(os.environ.get("FOOD10_GAP", "0.10"))
COLS = int(os.environ.get("FOOD10_COLS", "5"))
ROWS = int(os.environ.get("FOOD10_ROWS", "3"))
CELL = float(os.environ.get("PLATE_CELL", "36.0"))
FOOD = os.path.join(os.path.dirname(os.path.abspath(__file__)), "renders", "food")
OUT  = os.path.join(FOOD, "food10_plate.stl")

kw = dict(R=R, flare_up=FLARE_UP, wall=WALL, dome_r=gc.DOME_DIA / 2, dome_h=gc.DOME_HEIGHT,
          ridge_ir=gc.RIDGE_ID / 2, ridge_or=gc.RIDGE_OD / 2, ridge_h=gc.RIDGE_HEIGHT,
          peak_w=gc.RIDGE_PEAK_W, self_support=True, ss_deg=30.0, ss_clr=0.3)
prof, meta = sor.food(gap=GAP, **kw)

bpy.ops.wm.read_factory_settings(use_empty=True)
n = 0
for r in range(ROWS):
    for c in range(COLS):
        body = sor.revolve(prof, "food10_%02d" % n, seg=120)
        bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
        bpy.context.view_layer.objects.active = body
        bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
        zmin = min((body.matrix_world @ v.co).z for v in body.data.vertices)
        body.location = ((c - (COLS - 1) / 2) * CELL, (r - (ROWS - 1) / 2) * CELL, -zmin)
        n += 1

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
print("wrote %s  (%d food @ gap +%.2f, %dx%d grid @ %.0fmm, footprint ~%.0f x %.0f mm)"
      % (OUT, n, GAP, COLS, ROWS, CELL, (COLS - 1) * CELL + 30, (ROWS - 1) * CELL + 30))
