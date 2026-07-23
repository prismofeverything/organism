"""FDM tolerance-calibration ladder: 6 self-supporting food tokens at socket gaps +0.10..+0.35,
each with the gap value (x100) embossed on the top flare so they stay identifiable once handled.
All pegs are nominal, so test any token's SOCKET by seating it on any token's peg (or on a printed
piece). Pick the gap that seats snug-but-fully; that's your printer/material offset.

Run:  blender --background --python build_gap_ladder.py
Env:  LADDER_GAPS ("0.10,0.15,..."), PLATE_CELL (default 36)
"""
import bpy, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import graft_connector as gc
import sor

R, FLARE_UP, WALL = 13.0, 2.5, 2.5
FOOD = os.path.join(os.path.dirname(os.path.abspath(__file__)), "renders", "food")
OUT  = os.path.join(FOOD, "gap_ladder.stl")
GAPS = [float(x) for x in os.environ.get("LADDER_GAPS", "0.10,0.15,0.20,0.25,0.30,0.35").split(",")]
CELL = float(os.environ.get("PLATE_CELL", "36.0"))
RIDGE_OR = gc.RIDGE_OD / 2

kw = dict(R=R, flare_up=FLARE_UP, wall=WALL, dome_r=gc.DOME_DIA / 2, dome_h=gc.DOME_HEIGHT,
          ridge_ir=gc.RIDGE_ID / 2, ridge_or=RIDGE_OR, ridge_h=gc.RIDGE_HEIGHT,
          peak_w=gc.RIDGE_PEAK_W, self_support=True, ss_deg=30.0, ss_clr=0.3)

def emboss(food, txt, floor_z, r_lab=9.5):
    z = floor_z + FLARE_UP * ((r_lab - RIDGE_OR) / (R - RIDGE_OR)) ** 2   # flare surface at r_lab
    bpy.ops.object.text_add(location=(0.0, r_lab, z))
    t = bpy.context.object
    t.data.body = txt; t.data.align_x = 'CENTER'; t.data.align_y = 'CENTER'
    t.data.size = 3.2; t.data.extrude = 1.2                               # ~2.4mm thick -> proud of the sloped flare
    bpy.ops.object.convert(target='MESH')
    bpy.ops.object.select_all(action='DESELECT')
    t.select_set(True); food.select_set(True); bpy.context.view_layer.objects.active = food
    bpy.ops.object.join()                                                 # fuse label into the token (slicer unions overlap)

bpy.ops.wm.read_factory_settings(use_empty=True)
cells = [(c, r) for r in (0, 1) for c in (0, 1, 2)]
print("gap ladder (label = gap x100):")
for gap, (col, row) in zip(GAPS, cells):
    prof, meta = sor.food(gap=gap, **kw)
    food = sor.revolve(prof, "food_%02d" % round(gap * 100), seg=120)
    emboss(food, "%d" % round(gap * 100), meta["floor_z"])
    bpy.ops.object.select_all(action='DESELECT'); food.select_set(True)
    bpy.context.view_layer.objects.active = food
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min((food.matrix_world @ v.co).z for v in food.data.vertices)
    food.location = ((col - 1) * CELL, (row - 0.5) * CELL, -zmin)
    print("  gap %+0.2f  label '%d'  pos=(%+5.0f,%+5.0f)  height=%.2f"
          % (gap, round(gap * 100), (col - 1) * CELL, (row - 0.5) * CELL, meta["peg_tip"]))

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
print("wrote", OUT, "(%d tokens, gaps %s)" % (len(GAPS), GAPS))
