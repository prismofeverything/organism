"""Print plate: 3x FOOD_snap_ss + 3x FOOD_slip_ss (the self-supporting food), each dropped to the
bed (min z = 0, so socket-DOWN) and arranged in a 3x2 grid, exported as ONE STL.
Front row = snap (gap -0.10), back row = slip (gap +0.15).

Run:  blender --background --python build_food_plate_ss.py
Env:  PLATE_OUT (stl path), PLATE_CELL (grid spacing mm, default 40)
"""
import bpy, os, bmesh
HERE = os.path.dirname(os.path.abspath(__file__))
FOOD = os.path.join(HERE, "renders", "food")
OUT  = os.environ.get("PLATE_OUT", os.path.join(FOOD, "food_plate_ss.stl"))
CELL = float(os.environ.get("PLATE_CELL", "40.0"))

# (label, stl stem). Front row (y-) = snap, back row (y+) = slip.
items = [("snap-1", "FOOD_snap_ss"), ("snap-2", "FOOD_snap_ss"), ("snap-3", "FOOD_snap_ss"),
         ("slip-1", "FOOD_slip_ss"), ("slip-2", "FOOD_slip_ss"), ("slip-3", "FOOD_slip_ss")]
cells = [(c, r) for r in (0, 1) for c in (0, 1, 2)]   # 3 cols x 2 rows

bpy.ops.wm.read_factory_settings(use_empty=True)

def import_stl(path):
    before = set(bpy.data.objects)
    try: bpy.ops.wm.stl_import(filepath=path)
    except Exception: bpy.ops.import_mesh.stl(filepath=path)
    new = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
    if len(new) > 1:
        bpy.ops.object.select_all(action='DESELECT')
        for o in new: o.select_set(True)
        bpy.context.view_layer.objects.active = new[0]; bpy.ops.object.join(); new = [new[0]]
    return new[0]

print("food plate layout (non-manifold edges; 0 = watertight):")
for (label, stem), (col, row) in zip(items, cells):
    o = import_stl(os.path.join(FOOD, stem + ".stl")); o.name = label
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min((o.matrix_world @ v.co).z for v in o.data.vertices)
    o.location = ((col - 1) * CELL, (row - 0.5) * CELL, -zmin)   # centred grid + drop to bed
    bm = bmesh.new(); bm.from_mesh(o.data)
    nonman = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    print("  %-7s %-13s pos=(%+5.0f,%+5.0f)  non-manifold: %d"
          % (label, stem, (col - 1) * CELL, (row - 0.5) * CELL, nonman))

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
print("wrote %s  (6 food, ~%.0f x %.0f mm footprint)  FRONT row = snap(-0.10), BACK row = slip(+0.15)"
      % (OUT, 2 * CELL + 30, CELL + 30))
