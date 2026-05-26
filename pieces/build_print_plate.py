"""Lay out a single print plate: N of each food fit + 1 of each piece, each dropped onto
the bed (min z = 0) and arranged in a grid, exported as ONE STL for slicing. Light (import +
place + export, no render). See FOOD.md.

Run:  blender --background --python build_print_plate.py
Env:  PLATE_OUT (stl path), PLATE_CELL (grid spacing mm)
"""
import bpy, os, bmesh
HERE = os.path.dirname(os.path.abspath(__file__))
FOOD = os.path.join(HERE, "renders", "food")
OUT  = os.environ.get("PLATE_OUT", os.path.join(FOOD, "print_plate.stl"))
CELL = float(os.environ.get("PLATE_CELL", "42.0"))

# (label, path, kind). Pieces are GRAFT obj (body + universal connector, Z-up); food are
# revolved STL (Z-up). 3x3 grid: pieces back, slip food middle, snap food front.
items = [
    ("EAT",     f"{HERE}/out/EAT_graft.obj",       "obj"),
    ("MOVE",    f"{HERE}/out/MOVE_graft.obj",      "obj"),
    ("GROW",    f"{HERE}/out/GROW_graft.obj",      "obj"),
    ("slip-1",  f"{FOOD}/FOOD_slip.stl",           "stl"),
    ("slip-2",  f"{FOOD}/FOOD_slip.stl",           "stl"),
    ("slip-3",  f"{FOOD}/FOOD_slip.stl",           "stl"),
    ("snap-1",  f"{FOOD}/FOOD_snap.stl",           "stl"),
    ("snap-2",  f"{FOOD}/FOOD_snap.stl",           "stl"),
    ("snap-3",  f"{FOOD}/FOOD_snap.stl",           "stl"),
]
cells = [(c, r) for r in (2, 1, 0) for c in (0, 1, 2)]   # pieces back row, slip mid, snap front

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

print("plate layout (non-manifold edges; slicers auto-repair small counts):")
for (label, path, kind), (col, row) in zip(items, cells):
    o = import_one(path, kind); o.name = label
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min((o.matrix_world @ v.co).z for v in o.data.vertices)
    o.location = ((col - 1) * CELL, (row - 1) * CELL, -zmin)      # grid + drop to bed
    bm = bmesh.new(); bm.from_mesh(o.data)
    nonman = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    print(f"  {label:9s} pos=({(col-1)*CELL:+5.0f},{(row-1)*CELL:+5.0f})  non-manifold: {nonman}")

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
print("wrote", OUT, "(%d objects, ~%.0f mm square footprint)" % (len(items), 2 * CELL + 40))
