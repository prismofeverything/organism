"""Blender Workbench CAVITY renders of the MOVE graft (60mm tall, MOVE-framed):
side silhouette, half-cut profile, and an oblique crown close-up. Cavity shading
makes any crease/lump at the body<->cap<->connector joins shade dark.

  ~/Downloads/blender-5.1.1-linux-x64/blender -b --python render_move_graft.py -- out/MOVE_graft.obj
"""
import bpy, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
argf = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else ["out/MOVE_graft.obj"]
OBJ = os.path.join(HERE, argf[0])
stem = os.path.basename(OBJ).replace(".obj", "")


def fresh():
    bpy.ops.wm.read_factory_settings(use_empty=True)


def import_obj():
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=OBJ, up_axis='Z', forward_axis='Y')
    o = [ob for ob in bpy.data.objects if ob not in before and ob.type == 'MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    bpy.ops.object.shade_smooth()
    return o


def setup_scene(target_z, cam_loc, lens=70):
    sc = bpy.context.scene
    emp = bpy.data.objects.new("t", None); sc.collection.objects.link(emp)
    emp.location = (0, 0, target_z)
    camd = bpy.data.cameras.new("c"); cam = bpy.data.objects.new("c", camd)
    sc.collection.objects.link(cam); cam.location = cam_loc; camd.lens = lens
    con = cam.constraints.new('TRACK_TO'); con.target = emp
    con.track_axis = 'TRACK_NEGATIVE_Z'; con.up_axis = 'UP_Y'
    sc.camera = cam
    sc.render.engine = 'BLENDER_WORKBENCH'
    sh = sc.display.shading
    sh.light = 'STUDIO'; sh.show_shadows = True
    sh.show_cavity = True; sh.cavity_type = 'BOTH'
    sh.curvature_ridge_factor = 1.0; sh.curvature_valley_factor = 1.0
    sh.color_type = 'SINGLE'; sh.single_color = (0.8, 0.8, 0.85)
    sc.render.resolution_x = 1100; sc.render.resolution_y = 1300


def render(path):
    bpy.context.scene.render.filepath = path
    bpy.ops.render.render(write_still=True); print("wrote", path)


# --- 1. full side silhouette (see the flare + body) ---
fresh(); o = import_obj()
setup_scene(target_z=30, cam_loc=(0, -170, 33), lens=80)
render(os.path.join(HERE, "renders", f"cavity_{stem}_side.png"))

# --- 2. half-cut profile (bisect y=0, keep y<=0) ---
fresh(); o = import_obj()
bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
bpy.context.view_layer.objects.active = o
bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.bisect(plane_co=(0, 0, 0), plane_no=(0, 1, 0),
                    clear_inner=False, clear_outer=True, use_fill=True)
bpy.ops.object.mode_set(mode='OBJECT')
setup_scene(target_z=30, cam_loc=(0, -170, 33), lens=80)
render(os.path.join(HERE, "renders", f"cavity_{stem}_cut.png"))

# --- 3. oblique crown close-up (the connector + flare) ---
fresh(); o = import_obj()
setup_scene(target_z=54, cam_loc=(0, -42, 78), lens=110)
render(os.path.join(HERE, "renders", f"cavity_{stem}_crown.png"))

# --- 4. bottom-up view (the socket + base bulge) ---
fresh(); o = import_obj()
setup_scene(target_z=4, cam_loc=(0, -30, -34), lens=95)
render(os.path.join(HERE, "renders", f"cavity_{stem}_bottom.png"))
