"""Lay out a single print plate: N of each food fit + 1 of each piece, each dropped onto
the bed (min z = 0) and arranged in a grid, exported as ONE STL for slicing. Light (import +
place + export, no render). See FOOD.md.

Pieces: prefers the SCULPT grafts (out/*_sculpt_graft.obj — artist-finished Blender
sculpts with the connector grafted on by build_sculpt_graft.py). Falls back to the
parametric grafts (out/*_graft.obj) if the sculpt isn't present, or if USE_SCULPT=false.

PIECES_ONLY=true drops the food entirely and lays out just the 3 pieces (EAT/MOVE/GROW)
in a single row -> pieces_plate.stl. Food is a different filament colour, so it prints
on its own plate.

Run:  blender --background --python build_print_plate.py
Env:  PLATE_OUT (stl path), PLATE_CELL (grid spacing mm), USE_SCULPT (true/false),
      PIECES_ONLY (true/false -> pieces-only plate, no food)
"""
import bpy, os, bmesh
HERE = os.path.dirname(os.path.abspath(__file__))
FOOD = os.path.join(HERE, "renders", "food")
PIECES_ONLY = os.environ.get("PIECES_ONLY", "false").lower() in ("true", "1", "yes")
OUT  = os.environ.get("PLATE_OUT", os.path.join(
    FOOD, "pieces_plate.stl" if PIECES_ONLY else "print_plate.stl"))
CELL = float(os.environ.get("PLATE_CELL", "48.0" if PIECES_ONLY else "42.0"))
USE_SCULPT = os.environ.get("USE_SCULPT", "true").lower() in ("true", "1", "yes")

def piece_path(name):
    """Sculpt graft if present, else fall back to parametric graft."""
    sculpt = f"{HERE}/out/{name}_sculpt_graft.obj"
    parametric = f"{HERE}/out/{name}_graft.obj"
    if USE_SCULPT and os.path.exists(sculpt):
        return sculpt
    return parametric

# (label, path, kind). Pieces are GRAFT obj (body + universal connector, Z-up); food are
# revolved STL (Z-up).
pieces = [
    ("EAT",     piece_path("EAT"),                  "obj"),
    ("MOVE",    piece_path("MOVE"),                 "obj"),
    ("GROW",    piece_path("GROW"),                 "obj"),
]
food = [
    ("slip-1",  f"{FOOD}/FOOD_nosnap.stl",         "stl"),
    ("slip-2",  f"{FOOD}/FOOD_nosnap.stl",         "stl"),
    ("slip-3",  f"{FOOD}/FOOD_nosnap.stl",         "stl"),
    ("snap-1",  f"{FOOD}/FOOD_snap.stl",           "stl"),
    ("snap-2",  f"{FOOD}/FOOD_snap.stl",           "stl"),
    ("snap-3",  f"{FOOD}/FOOD_snap.stl",           "stl"),
]
if PIECES_ONLY:                                          # pieces-only plate: single centered row
    items = pieces
    positions = [(-CELL, 0.0), (0.0, 0.0), (CELL, 0.0)]
else:                                                    # full 3x3: pieces back, slip mid, snap front
    items = pieces + food
    positions = [((c - 1) * CELL, (r - 1) * CELL) for r in (2, 1, 0) for c in (0, 1, 2)]
print(f"using sculpt grafts: {USE_SCULPT} (fall back to parametric if sculpt missing)")
for label, path, _ in items[:3]:
    flavor = "SCULPT" if "sculpt" in path else "PARAMETRIC"
    print(f"  {label}: {flavor}  {os.path.basename(path)}")

bpy.ops.wm.read_factory_settings(use_empty=True)

def import_one(path, kind):
    before = set(bpy.data.objects)
    if kind == "obj":
        bpy.ops.wm.obj_import(filepath=path, forward_axis='Y', up_axis='Z')
    else:
        try: bpy.ops.wm.stl_import(filepath=path)
        except Exception: bpy.ops.import_mesh.stl(filepath=path)
    new = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
    if len(new) > 1:
        bpy.ops.object.select_all(action='DESELECT')
        for o in new: o.select_set(True)
        bpy.context.view_layer.objects.active = new[0]; bpy.ops.object.join(); new = [new[0]]
    return new[0]

# Degenerate sliver removal happens after export, in strip_slivers.py (trimesh isolates the
# near-coincident sculpt specks as their own tiny bodies; Blender welds them onto the body here,
# so they can't be separated at this stage).
print("plate layout (non-manifold edges; slicers auto-repair small counts):")
for (label, path, kind), (x, y) in zip(items, positions):
    o = import_one(path, kind); o.name = label
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min((o.matrix_world @ v.co).z for v in o.data.vertices)
    o.location = (x, y, -zmin)                                    # grid + drop to bed
    bm = bmesh.new(); bm.from_mesh(o.data)
    nonman = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    print(f"  {label:9s} pos=({x:+5.0f},{y:+5.0f})  non-manifold: {nonman}")

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
foot = ("~%.0f mm wide x ~40 mm deep row" if PIECES_ONLY else "~%.0f mm square") % (2 * CELL + 40)
print("wrote", OUT, "(%d objects, %s footprint)" % (len(items), foot))
