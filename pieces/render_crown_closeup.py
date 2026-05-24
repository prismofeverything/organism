"""Close-up of piece crowns (Workbench + cavity). Usage:
   blender -b --python render_crown_closeup.py -- a.obj b.obj   (default: the seamless EAT+MOVE)"""
import bpy, os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
argf = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else ["renders/EAT_seamless.obj", "renders/MOVE_seamless.obj"]
items = [(os.path.basename(a).replace(".obj", ""), a) for a in argf]
tag = "_".join(n for n, _ in items)
OUT = os.path.join(HERE, "renders/closeup_" + tag + ".png")
xs = [(i - (len(items) - 1) / 2) * 48 for i in range(len(items))]
bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
mat = bpy.data.materials.new("m"); mat.diffuse_color = (0.82, 0.82, 0.86, 1)
for (name, path), x in zip(items, xs):
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=os.path.join(HERE, path), up_axis='Y', forward_axis='NEGATIVE_Z')
    o = [ob for ob in bpy.data.objects if ob not in before and ob.type == 'MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min(v.co.z for v in o.data.vertices); o.location = (x, 0, -zmin)
    bpy.ops.object.shade_smooth(); o.data.materials.append(mat)
emp = bpy.data.objects.new("t", None); sc.collection.objects.link(emp); emp.location = (0, 0, 47)
camd = bpy.data.cameras.new("c"); cam = bpy.data.objects.new("c", camd); sc.collection.objects.link(cam)
cam.location = (0, -135, 74); camd.lens = 90
con = cam.constraints.new('TRACK_TO'); con.target = emp; con.track_axis = 'TRACK_NEGATIVE_Z'; con.up_axis = 'UP_Y'
sc.camera = cam
sc.render.engine = 'BLENDER_WORKBENCH'; sh = sc.display.shading
sh.light = 'STUDIO'; sh.show_shadows = True; sh.show_cavity = True; sh.cavity_type = 'BOTH'
sh.color_type = 'SINGLE'; sh.single_color = (0.8, 0.8, 0.85)
sc.render.resolution_x = 1400; sc.render.resolution_y = 760; sc.render.filepath = OUT
bpy.ops.render.render(write_still=True); print("wrote", OUT)
