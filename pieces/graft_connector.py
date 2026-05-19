"""Graft the parametric universal connector (parabolic dome + outer ridge on top,
matching socket cavity on bottom) onto a sculpted body mesh.

Use AFTER you've sculpted the body in Blender and exported a clean OBJ. This
script attaches the connector with exact parametric dimensions, so sculpting
freely is safe — your changes never touch the connector geometry.

Usage:
  ~/Downloads/blender-5.1.1-linux-x64/blender --background --python graft_connector.py -- \
       --in MOVE_sculpt_v2.obj --piece MOVE --out MOVE_final.obj

  Flags:
    --in     path to sculpted OBJ
    --piece  one of {EAT, MOVE, GROW, FOOD}  (controls socket-on-bottom yes/no)
    --out    output path (default: <input>_connected.obj)
    --z      override top-plateau Z; default: max Z of input mesh
"""
import bpy, bmesh, sys, math, json, argparse
from math import pi, cos, sin
from pathlib import Path

HERE = Path(__file__).parent

# Same spec as pieces_v2.py — keep in sync.
DOME_DIA       = 6.0
DOME_HEIGHT    = 3.0
RIDGE_OD       = 12.0
RIDGE_ID       = 9.0
RIDGE_HEIGHT   = 0.6
RIDGE_PEAK_W   = 1.0
CLEARANCE      = 0.10
DOME_SEGS      = 64
DOME_RES       = 24


def parse_args():
    # Blender stops at "--" and passes everything after to the script
    argv = sys.argv
    if "--" in argv:
        argv = argv[argv.index("--")+1:]
    else:
        argv = []
    p = argparse.ArgumentParser()
    p.add_argument("--in", dest="input", required=True)
    p.add_argument("--piece", required=True, choices=["EAT", "MOVE", "GROW", "FOOD"])
    p.add_argument("--out", default=None)
    p.add_argument("--z", type=float, default=None)
    return p.parse_args(argv)


def build_parabolic_dome(name, r_base, height, segments=DOME_SEGS, n_z=DOME_RES, z_offset=0):
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    rings = []
    for iz in range(n_z + 1):
        t = iz / n_z
        z = t * height
        r = r_base * math.sqrt(max(0.0, 1.0 - t))
        if r < 1e-4:
            apex = bm.verts.new((0, 0, z_offset + z))
            rings.append(("apex", apex))
            break
        ring = [bm.verts.new((r*cos(2*pi*ith/segments), r*sin(2*pi*ith/segments), z_offset + z))
                for ith in range(segments)]
        rings.append(("ring", ring))
    for i in range(len(rings)-1):
        ak, a = rings[i]; bk, b = rings[i+1]
        if ak=="ring" and bk=="ring":
            for ith in range(segments):
                j = (ith+1) % segments
                bm.faces.new([a[ith], a[j], b[j], b[ith]])
        elif ak=="ring" and bk=="apex":
            for ith in range(segments):
                j = (ith+1) % segments
                bm.faces.new([a[ith], a[j], b])
    bm.faces.new(rings[0][1][::-1])
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def build_ridge_ring(name, od, id_, height, peak_w, segments=DOME_SEGS, z_offset=0):
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    n_profile = 12
    profile = []
    for ip in range(n_profile+1):
        u = ip / n_profile
        r = id_/2 + (od/2 - id_/2) * u
        u_centered = abs(2*u - 1)
        center_frac = peak_w / (od - id_)
        edge_frac = max(1 - center_frac, 1e-6)
        if u_centered < center_frac:
            bump = 1.0
        else:
            t = (u_centered - center_frac) / edge_frac
            bump = 1.0 - t*t
        profile.append((r, height * bump))
    rings_top = []
    for r, z in profile:
        ring = [bm.verts.new((r*cos(2*pi*ith/segments), r*sin(2*pi*ith/segments), z_offset + z))
                for ith in range(segments)]
        rings_top.append(ring)
    bot_inner = [bm.verts.new(((id_/2)*cos(2*pi*ith/segments), (id_/2)*sin(2*pi*ith/segments), z_offset))
                 for ith in range(segments)]
    bot_outer = [bm.verts.new(((od/2)*cos(2*pi*ith/segments), (od/2)*sin(2*pi*ith/segments), z_offset))
                 for ith in range(segments)]
    for i in range(len(rings_top)-1):
        a = rings_top[i]; b = rings_top[i+1]
        for ith in range(segments):
            j = (ith+1) % segments
            bm.faces.new([a[ith], a[j], b[j], b[ith]])
    for ith in range(segments):
        j = (ith+1) % segments
        bm.faces.new([bot_inner[ith], bot_outer[ith], bot_outer[j], bot_inner[j]])
    inner_top = rings_top[0]; outer_top = rings_top[-1]
    for ith in range(segments):
        j = (ith+1) % segments
        bm.faces.new([inner_top[ith], inner_top[j], bot_inner[j], bot_inner[ith]])
        bm.faces.new([outer_top[j], outer_top[ith], bot_outer[ith], bot_outer[j]])
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def add_peg(parent, top_z):
    dome  = build_parabolic_dome("Dome",  DOME_DIA/2, DOME_HEIGHT, z_offset=top_z)
    ridge = build_ridge_ring("Ridge", RIDGE_OD, RIDGE_ID, RIDGE_HEIGHT, RIDGE_PEAK_W, z_offset=top_z)
    for child in (dome, ridge):
        bpy.context.view_layer.objects.active = parent
        m = parent.modifiers.new(f"U_{child.name}", 'BOOLEAN')
        m.operation='UNION'; m.object=child; m.solver='EXACT'
        bpy.ops.object.modifier_apply(modifier=f"U_{child.name}")
        bpy.data.objects.remove(child, do_unlink=True)


def add_socket(parent):
    dome  = build_parabolic_dome("CavDome",  DOME_DIA/2 + CLEARANCE, DOME_HEIGHT + CLEARANCE, z_offset=0)
    dome.scale.z = -1; bpy.context.view_layer.objects.active = dome
    bpy.ops.object.transform_apply(scale=True)
    ridge = build_ridge_ring("CavRidge", RIDGE_OD + 2*CLEARANCE, RIDGE_ID - 2*CLEARANCE,
                             RIDGE_HEIGHT + CLEARANCE, RIDGE_PEAK_W, z_offset=0)
    ridge.scale.z = -1; bpy.context.view_layer.objects.active = ridge
    bpy.ops.object.transform_apply(scale=True)
    for child in (dome, ridge):
        bpy.context.view_layer.objects.active = parent
        m = parent.modifiers.new(f"C_{child.name}", 'BOOLEAN')
        m.operation='DIFFERENCE'; m.object=child; m.solver='EXACT'
        bpy.ops.object.modifier_apply(modifier=f"C_{child.name}")
        bpy.data.objects.remove(child, do_unlink=True)


def main():
    args = parse_args()
    in_path = Path(args.input).resolve()
    if not in_path.exists():
        print(f"ERROR: input not found: {in_path}")
        sys.exit(1)

    bpy.ops.wm.read_factory_settings(use_empty=True)
    bpy.ops.wm.obj_import(filepath=str(in_path), forward_axis='NEGATIVE_Z', up_axis='Y')
    meshes = [o for o in bpy.context.scene.objects if o.type=='MESH']
    bpy.ops.object.select_all(action='DESELECT')
    for o in meshes: o.select_set(True)
    bpy.context.view_layer.objects.active = meshes[0]
    if len(meshes) > 1: bpy.ops.object.join()
    body = bpy.context.view_layer.objects.active
    body.name = args.piece

    # Determine peg insertion z. Default: max Z of mesh (where the plateau is).
    if args.z is not None:
        peg_top_z = args.z
    else:
        bb = [body.matrix_world @ v.co for v in body.data.vertices]
        peg_top_z = max(v.z for v in bb)
    print(f"  peg base at z = {peg_top_z:.2f} mm")

    add_peg(body, peg_top_z)

    # FOOD/MOVE/GROW get the bottom socket; EAT doesn't (food never goes UNDER EAT)
    if args.piece != "EAT":
        add_socket(body)
        print(f"  socket carved at z = 0")

    out_path = Path(args.out) if args.out else in_path.parent / f"{in_path.stem}_connected.obj"
    bpy.ops.object.select_all(action='DESELECT')
    body.select_set(True); bpy.context.view_layer.objects.active = body
    bpy.ops.wm.obj_export(filepath=str(out_path), export_selected_objects=True,
                          forward_axis='NEGATIVE_Z', up_axis='Y', export_materials=False)
    print(f"\n  → {out_path}")


if __name__ == "__main__":
    main()
