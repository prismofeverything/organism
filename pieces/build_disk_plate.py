"""Print plate of the 2D disks (EAT / MOVE / GROW), laid out in a grid and dropped onto the bed
socket-DOWN (peg up). DISK_COPIES copies of each type -> one row per type, DISK_COPIES columns.
The disks have self-supporting sockets (build_minimal.py + sor._socket_ss), so this prints with NO
support inside the socket and NO support outside (flat bottom). Exported as ONE STL.

Run:  blender --background --python build_disk_plate.py
Env:  DISK_COPIES (per type, default 4), DISK_PLATE_OUT (stl path, default stl/disk_plate.stl),
      PLATE_CELL (grid pitch mm, default 45)
"""
import bpy, os, bmesh
HERE = os.path.dirname(os.path.abspath(__file__))
OUT    = os.environ.get("DISK_PLATE_OUT", os.path.join(HERE, "stl", "disk_plate.stl"))
CELL   = float(os.environ.get("PLATE_CELL", "45.0"))
COPIES = int(os.environ.get("DISK_COPIES", "4"))

types = [("EAT",  f"{HERE}/out/EAT_mindisk.obj"),
         ("MOVE", f"{HERE}/out/MOVE_mindisk.obj"),
         ("GROW", f"{HERE}/out/GROW_mindisk.obj")]
rows = len(types)

bpy.ops.wm.read_factory_settings(use_empty=True)

def import_one(path):
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=path, forward_axis='Y', up_axis='Z')
    new = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
    if len(new) > 1:
        bpy.ops.object.select_all(action='DESELECT')
        for o in new: o.select_set(True)
        bpy.context.view_layer.objects.active = new[0]; bpy.ops.object.join(); new = [new[0]]
    return new[0]

# one row per type (EAT / MOVE / GROW), COPIES columns; grid centered on the origin
print(f"disk plate: {COPIES} each of EAT/MOVE/GROW = {COPIES*rows} disks, {CELL:.0f}mm pitch, socket-down")
placed = 0
for ti, (name, path) in enumerate(types):
    y = ((rows - 1) / 2 - ti) * CELL
    for ci in range(COPIES):
        x = (ci - (COPIES - 1) / 2) * CELL
        o = import_one(path); o.name = f"{name}_{ci+1}"
        bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
        bpy.context.view_layer.objects.active = o
        bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
        zmin = min((o.matrix_world @ v.co).z for v in o.data.vertices)
        o.location = (x, y, -zmin)                                # grid + drop to bed
        placed += 1

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='SELECT')
try: bpy.ops.wm.stl_export(filepath=OUT, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=OUT, use_selection=True)
gx = (COPIES - 1) * CELL + 40; gy = (rows - 1) * CELL + 40
print(f"wrote {OUT} ({placed} disks, ~{gx:.0f} x {gy:.0f} mm)")
