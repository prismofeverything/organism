"""Blender bmesh manifold check (the reliable one). Usage:
   blender -b --python check_manifold.py -- a.obj b.obj ..."""
import bpy, bmesh, sys
files = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
for f in files:
    bpy.ops.wm.read_factory_settings(use_empty=True)
    bpy.ops.wm.obj_import(filepath=f, up_axis='Y', forward_axis='NEGATIVE_Z')
    o = [x for x in bpy.context.scene.objects if x.type == 'MESH'][0]
    bm = bmesh.new(); bm.from_mesh(o.data)
    nm = sum(1 for e in bm.edges if not e.is_manifold)
    print("MANIFOLD %s : non-manifold edges = %d  (verts %d)" % (f.split('/')[-1], nm, len(bm.verts)))
    bm.free()
