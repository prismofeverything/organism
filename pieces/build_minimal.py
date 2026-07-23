"""MINIMAL pieces: the 2D-silhouette stand-ins for the 3D EAT/MOVE/GROW.

Each is the piece's top-down SVG silhouette (inputs/{eat,move,grow}.svg -> the same source
the 3D pieces are built from) scaled to the 37 mm footprint, extruded to a 9 mm disk with the
silhouette edges BEVELLED above and below (continuous, no sharp corners). They are "basically
food": the universal connector PEG (dome+ridge) sits on top and a matching SOCKET is carved into
the bottom, so a disk nests onto a piece/food peg and food nests onto it.

  ~/Downloads/.../blender -b --python build_minimal.py           # -> out/{EAT,MOVE,GROW}_mindisk.obj(+stl)
Env: MIN_FP (footprint mm, def 37), MIN_H (body height mm, def 9), MIN_BEVEL (edge bevel mm, def 0.7)
"""
import bpy, bmesh, math, os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import graft_connector as gc          # universal connector: dims + peg/socket builders
try: bpy.ops.preferences.addon_enable(module='io_curve_svg')   # SVG import (headless-safe)
except Exception: pass

FP    = float(os.environ.get("MIN_FP", "37.0"))
H     = float(os.environ.get("MIN_H", "9.0"))
BEVEL = float(os.environ.get("MIN_BEVEL", "0.7"))
PIECES = [("EAT", "eat"), ("MOVE", "move"), ("GROW", "grow")]

def import_silhouette(svg):
    before = set(bpy.data.objects)
    bpy.ops.import_curve.svg(filepath=svg)
    curves = [o for o in bpy.data.objects if o not in before and o.type == 'CURVE']
    bpy.ops.object.select_all(action='DESELECT')
    for o in curves: o.select_set(True)
    bpy.context.view_layer.objects.active = curves[0]
    if len(curves) > 1: bpy.ops.object.join()
    return curves[0]

def union_cylinder(body, r, h):
    """Union a central cylinder so there's solid material under the connector (also fills
    GROW's central ring hole, which is what made it non-manifold)."""
    bpy.ops.mesh.primitive_cylinder_add(vertices=64, radius=r, depth=h, location=(0, 0, h/2))
    cyl = bpy.context.active_object
    bpy.context.view_layer.objects.active = body
    m = body.modifiers.new("U_core", 'BOOLEAN'); m.operation = 'UNION'; m.object = cyl; m.solver = 'EXACT'
    bpy.ops.object.modifier_apply(modifier="U_core")
    bpy.data.objects.remove(cyl, do_unlink=True)

def cleanup(body):
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.remove_doubles(threshold=1e-4)
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.object.mode_set(mode='OBJECT')
    bm = bmesh.new(); bm.from_mesh(body.data); nm = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    if nm > 0:                                    # heal residual self-intersections -> watertight
        body.data.remesh_voxel_size = 0.18
        try: body.data.use_remesh_smooth_normals = True
        except Exception: pass
        bpy.ops.object.voxel_remesh(); bpy.ops.object.shade_smooth()
        print(f"  {body.name}: {nm} non-manifold -> voxel remesh 0.18mm")
    return nm

def build(name, stem):
    bpy.ops.wm.read_factory_settings(use_empty=True)
    cur = import_silhouette(f"{HERE}/inputs/{stem}.svg")
    bpy.ops.object.select_all(action='DESELECT'); cur.select_set(True)
    bpy.context.view_layer.objects.active = cur
    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY', center='BOUNDS'); cur.location = (0, 0, 0)
    w = max(cur.dimensions.x, cur.dimensions.y)
    s = (FP - 2*BEVEL) / w                        # bevel grows the outline by ~BEVEL/side -> compensate
    cd = cur.data
    cd.dimensions = '2D'; cd.fill_mode = 'BOTH'
    cd.extrude = (H/2 - BEVEL) / s
    cd.bevel_depth = BEVEL / s; cd.bevel_resolution = 3; cd.resolution_u = 8
    cur.scale = (s, -s, s)                         # -Y: SVG y-down -> match the piece silhouette
    bpy.ops.object.convert(target='MESH')          # bake curve geometry (can't apply xf to a 2D curve)
    body = bpy.context.view_layer.objects.active
    body.location.z = H/2
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.normals_make_consistent(inside=False); bpy.ops.object.mode_set(mode='OBJECT')
    # food-style connector
    union_cylinder(body, gc.RIDGE_OD/2 + 0.8, H)   # solid core for the peg/socket (fixes GROW)
    gc.add_peg(body, H)                            # peg (dome+ridge) on top
    gc.add_socket(body)                            # matching socket carved into the bottom
    nm = cleanup(body)
    bpy.ops.object.shade_smooth()
    zr = [v.co.z for v in body.data.vertices]
    print(f"{name}: footprint {max(body.dimensions.x, body.dimensions.y):.1f} mm  "
          f"body+peg height {max(zr)-min(zr):.2f} mm  non-manifold {nm}")
    out = f"{HERE}/out/{name}_mindisk"
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    bpy.ops.wm.obj_export(filepath=out + ".obj", export_selected_objects=True, forward_axis='Y', up_axis='Z')
    try: bpy.ops.wm.stl_export(filepath=out + ".stl", export_selected_objects=True)
    except Exception: bpy.ops.export_mesh.stl(filepath=out + ".stl", use_selection=True)
    print("  wrote", out + ".obj (+.stl)")

os.makedirs(f"{HERE}/out", exist_ok=True)
for name, stem in PIECES:
    build(name, stem)
print("minimal disks done")
