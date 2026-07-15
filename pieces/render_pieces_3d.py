"""Quick 3D look at the pieces (Workbench + cavity, so creases/facets show dark). Gentle.
Run: blender -b --threads 2 --python render_pieces_3d.py"""
import bpy, os, math
HERE = os.path.dirname(os.path.abspath(__file__))
items = [("EAT old", "EAT_connected.obj"), ("EAT new", "renders/EAT_seamless.obj"),
         ("MOVE", "renders/MOVE_seamless.obj"), ("GROW", "renders/GROW_seamless.obj")]
OUT = os.path.join(HERE, "renders/seamless_3d.png")

bpy.ops.wm.read_factory_settings(use_empty=True)
sc = bpy.context.scene
mat = bpy.data.materials.new("m"); mat.diffuse_color = (0.82, 0.82, 0.86, 1)
xs = [-66, -22, 22, 66]
for (name, path), x in zip(items, xs):
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=os.path.join(HERE, path), up_axis='Y', forward_axis='NEGATIVE_Z')
    o = [ob for ob in bpy.data.objects if ob not in before and ob.type == 'MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min(v.co.z for v in o.data.vertices)
    o.location = (x, 0, -zmin); bpy.ops.object.shade_smooth(); o.data.materials.append(mat)

emp = bpy.data.objects.new("t", None); sc.collection.objects.link(emp); emp.location = (0, 0, 27)
camd = bpy.data.cameras.new("c"); cam = bpy.data.objects.new("c", camd); sc.collection.objects.link(cam)
cam.location = (0, -320, 110); camd.lens = 44
con = cam.constraints.new('TRACK_TO'); con.target = emp; con.track_axis = 'TRACK_NEGATIVE_Z'; con.up_axis = 'UP_Y'
sc.camera = cam

sc.render.engine = 'BLENDER_WORKBENCH'
sh = sc.display.shading
sh.light = 'STUDIO'; sh.show_shadows = True
sh.show_cavity = True; sh.cavity_type = 'BOTH'
sh.color_type = 'SINGLE'; sh.single_color = (0.8, 0.8, 0.85)
sc.render.resolution_x = 1500; sc.render.resolution_y = 520
sc.render.filepath = OUT
bpy.ops.render.render(write_still=True)
print("wrote", OUT)
