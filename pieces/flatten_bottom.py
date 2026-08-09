"""Flatten a piece's bottom by a horizontal cut, for stability + bed adhesion.
Bisects the mesh at z = zmin + FLAT_MM, discards everything below, caps the cut flat, re-zeros to the
bed. The top (connector) is untouched, so mating is unchanged; the piece just gets FLAT_MM shorter.

Run:  blender -b --python flatten_bottom.py
Env:  FLAT_IN (piece OBJ), FLAT_MM (cut depth mm), FLAT_OUT (default = FLAT_IN, in place)
"""
import bpy, bmesh, os
INP = os.environ["FLAT_IN"]; X = float(os.environ["FLAT_MM"]); OUT = os.environ.get("FLAT_OUT", INP)

bpy.ops.wm.read_factory_settings(use_empty=True)
before = set(bpy.data.objects)
bpy.ops.wm.obj_import(filepath=INP, up_axis='Z', forward_axis='Y')
objs = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
bpy.ops.object.select_all(action='DESELECT')
for o in objs: o.select_set(True)
bpy.context.view_layer.objects.active = objs[0]
if len(objs) > 1: bpy.ops.object.join()
o = bpy.context.view_layer.objects.active
bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
z0 = min(v.co.z for v in o.data.vertices)

# bisect at z0+X: keep above (clear_inner removes the -normal side = below), cap the cut
bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.bisect(plane_co=(0, 0, z0 + X), plane_no=(0, 0, 1),
                    clear_inner=True, clear_outer=False, use_fill=True)
bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.remove_doubles(threshold=1e-4)
bpy.ops.mesh.normals_make_consistent(inside=False)
bpy.ops.object.mode_set(mode='OBJECT')

# re-zero to the bed
zmin = min(v.co.z for v in o.data.vertices); o.location.z = -zmin
bpy.ops.object.transform_apply(location=True)
bm = bmesh.new(); bm.from_mesh(o.data); nm = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
zr = [v.co.z for v in o.data.vertices]
print(f"flatten {os.path.basename(INP)} -{X}mm: height {max(zr)-min(zr):.1f}mm  non-manifold {nm}")

bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active = o
bpy.ops.wm.obj_export(filepath=OUT, up_axis='Z', forward_axis='Y', export_selected_objects=True,
                      export_materials=False)
stl = OUT.replace('.obj', '.stl')
try: bpy.ops.wm.stl_export(filepath=stl, export_selected_objects=True)
except Exception: bpy.ops.export_mesh.stl(filepath=stl, use_selection=True)
print(f"wrote {OUT} (+{stl})")
