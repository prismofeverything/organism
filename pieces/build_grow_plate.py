"""Print plate of N copies of a 3D player piece (default GROW), in a grid, dropped to the bed peg-up.
These are the tall sculpted grafts — slice them WITH supports (overhanging lobes) + a brim.

Run:  blender --background --python build_grow_plate.py
Env:  PIECE_OBJ (default out/GROW_sculpt_graft.obj), PIECE_COPIES (default 4),
      PIECE_PLATE_OUT (default stl/grow_plate.stl), PLATE_CELL (grid pitch mm, default 48)
"""
import bpy, os, math, bmesh
HERE = os.path.dirname(os.path.abspath(__file__))
OBJ    = os.environ.get("PIECE_OBJ", f"{HERE}/out/GROW_sculpt_graft.obj")
COPIES = int(os.environ.get("PIECE_COPIES", "4"))
OUT    = os.environ.get("PIECE_PLATE_OUT", f"{HERE}/stl/grow_plate.stl")
CELL   = float(os.environ.get("PLATE_CELL", "48.0"))

cols = math.ceil(math.sqrt(COPIES)); rows = math.ceil(COPIES / cols)
bpy.ops.wm.read_factory_settings(use_empty=True)

def import_one(path):
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    new = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
    if len(new) > 1:
        bpy.ops.object.select_all(action='DESELECT')
        for o in new: o.select_set(True)
        bpy.context.view_layer.objects.active = new[0]; bpy.ops.object.join(); new = [new[0]]
    return new[0]

name = os.path.splitext(os.path.basename(OBJ))[0]
print(f"plate: {COPIES}x {name}, {cols}x{rows} grid, {CELL:.0f}mm pitch, peg-up")
placed = 0
for i in range(COPIES):
    c, r = i % cols, i // cols
    x = (c - (cols - 1) / 2) * CELL
    y = ((rows - 1) / 2 - r) * CELL
    o = import_one(OBJ); o.name = f"{name}_{i+1}"
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min((o.matrix_world @ v.co).z for v in o.data.vertices)
    o.location = (x, y, -zmin)
    bm = bmesh.new(); bm.from_mesh(o.data); nm = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    print(f"  {o.name} pos=({x:+5.0f},{y:+5.0f}) non-manifold {nm}")
    placed += 1

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
print(f"wrote {OUT} ({placed} pieces, ~{(cols-1)*CELL+40:.0f} x {(rows-1)*CELL+40:.0f} mm)")
