"""Blender Workbench CAVITY render of the EAT graft — the definitive seam judge
(concavities shade dark, so any lump/crease/shelf at the connector<->body join
shows immediately). Renders an oblique crown close-up and a half-cut profile.

  ~/Downloads/blender-5.1.1-linux-x64/blender -b --python render_graft_blender.py -- out/EAT_graft.obj
"""
import bpy, os, sys, math

HERE = os.path.dirname(os.path.abspath(__file__))
argf = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else ["out/EAT_graft.obj"]
OBJ = os.path.join(HERE, argf[0])
stem = os.path.basename(OBJ).replace(".obj", "")


def fresh():
    bpy.ops.wm.read_factory_settings(use_empty=True)


def import_obj():
    before = set(bpy.data.objects)
    # meshlib OBJ is Z-up already -> up_axis='Z' so Blender doesn't re-rotate
    bpy.ops.wm.obj_import(filepath=OBJ, up_axis='Z', forward_axis='Y')
    o = [ob for ob in bpy.data.objects if ob not in before and ob.type == 'MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    bpy.ops.object.shade_smooth()
    mat = bpy.data.materials.new("m"); mat.diffuse_color = (0.82, 0.82, 0.86, 1)
    o.data.materials.append(mat)
    return o


def setup_scene(target_z, cam_loc, lens=85):
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
    sc.render.resolution_x = 1200; sc.render.resolution_y = 1000


def render(path):
    bpy.context.scene.render.filepath = path
    bpy.ops.render.render(write_still=True); print("wrote", path)


# --- 1. crown close-up (cavity) ---
fresh(); o = import_obj()
setup_scene(target_z=44, cam_loc=(0, -34, 60), lens=110)
render(os.path.join(HERE, "renders", f"cavity_{stem}_crown.png"))

# --- 2. half-cut profile (bisect at y=0, keep y<=0, view the cut) ---
fresh(); o = import_obj()
bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
bpy.context.view_layer.objects.active = o
bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.bisect(plane_co=(0, 0, 0), plane_no=(0, 1, 0),
                    clear_inner=False, clear_outer=True, use_fill=True)
bpy.ops.object.mode_set(mode='OBJECT')
setup_scene(target_z=30, cam_loc=(0, -120, 36), lens=70)
render(os.path.join(HERE, "renders", f"cavity_{stem}_cut.png"))
