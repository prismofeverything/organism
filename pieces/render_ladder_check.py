"""Top-down Workbench render of the gap ladder to confirm the embossed labels are legible.
Run:  blender --background --python render_ladder_check.py"""
import bpy, os, math
HERE = os.path.dirname(os.path.abspath(__file__))
STL  = os.path.join(HERE, "renders/food/gap_ladder.stl")
OUT  = os.path.join(HERE, "renders/food/gap_ladder_top.png")

bpy.ops.wm.read_factory_settings(use_empty=True)
try: bpy.ops.wm.stl_import(filepath=STL)
except Exception: bpy.ops.import_mesh.stl(filepath=STL)

# frame from the object bounds
obs = [o for o in bpy.data.objects if o.type == 'MESH']
xs = [ (o.matrix_world @ v.co) for o in obs for v in o.data.vertices ]
minx = min(p.x for p in xs); maxx = max(p.x for p in xs)
miny = min(p.y for p in xs); maxy = max(p.y for p in xs)
cx, cy = (minx+maxx)/2, (miny+maxy)/2
span = max(maxx-minx, maxy-miny) * 1.1

cam_data = bpy.data.cameras.new("cam"); cam_data.type = 'ORTHO'; cam_data.ortho_scale = span
cam = bpy.data.objects.new("cam", cam_data); bpy.context.collection.objects.link(cam)
cam.location = (cx, cy, 120); cam.rotation_euler = (0, 0, 0)
bpy.context.scene.camera = cam

sun = bpy.data.objects.new("sun", bpy.data.lights.new("sun", 'SUN'))
sun.data.energy = 4.0; sun.location = (cx+40, cy-40, 120); sun.rotation_euler = (math.radians(25), math.radians(15), 0)
bpy.context.collection.objects.link(sun)

sc = bpy.context.scene
sc.render.engine = 'BLENDER_WORKBENCH'
sc.display.shading.light = 'STUDIO'; sc.display.shading.show_cavity = True
sc.render.resolution_x = 1300; sc.render.resolution_y = 900
sc.render.film_transparent = False
sc.render.filepath = OUT
bpy.ops.render.render(write_still=True)
print("wrote", OUT)
