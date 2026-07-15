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
    --fuse   seat the connector on a flat O~16 plateau (at the height where the
             body is already that wide) and fillet the junction so it reads as
             grown-in, not stuck-on. Reshapes the narrow apex into a plateau.
             This is the look you want on the final sculpted bodies.
    --voxel  fuse remesh voxel size in mm (default 0.30; smaller = more detail)
"""
import bpy, bmesh, sys, math, json, argparse
import numpy as np
from math import pi, cos, sin
from pathlib import Path

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE))     # so `import sor` resolves under Blender's bundled Python

# Same spec as pieces_v2.py — keep in sync.
DOME_DIA       = 3.8
DOME_HEIGHT    = 4.3
RIDGE_OD       = 12.8
RIDGE_ID       = 8.3
RIDGE_HEIGHT   = 2.75
RIDGE_PEAK_W   = 2.0
CLEARANCE      = 0.15      # nominal slip-fit gap (peg <-> socket)
IM_TOLERANCE   = 0.05      # extra allowance for injection-molding dimensional tolerance
SOCKET_GAP     = CLEARANCE + IM_TOLERANCE   # total socket oversize
SEAT_MARGIN    = 0.6       # plateau radius past the connector base, so it seats flush
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
    p.add_argument("--fuse", action="store_true",
                   help="seat on a flat plateau + fillet the junction (grown-in look)")
    p.add_argument("--voxel", type=float, default=0.30, help="fuse remesh voxel size (mm)")
    p.add_argument("--seamless", action="store_true",
                   help="domed crown (no flat shelf): grow the peg out of the body via a revolved sor meridian")
    p.add_argument("--height", type=float, default=None,
                   help="target final piece height (mm); connector base z = height - DOME_HEIGHT")
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
    # Cavity opens at the bottom face (z=0) and narrows upward into the body, so the
    # peg of the piece below seats into it. SOCKET_GAP = fit clearance + IM tolerance.
    dome  = build_parabolic_dome("CavDome",  DOME_DIA/2 + SOCKET_GAP, DOME_HEIGHT + SOCKET_GAP, z_offset=0)
    ridge = build_ridge_ring("CavRidge", RIDGE_OD + 2*SOCKET_GAP, RIDGE_ID - 2*SOCKET_GAP,
                             RIDGE_HEIGHT + SOCKET_GAP, RIDGE_PEAK_W, z_offset=0)
    for child in (dome, ridge):
        bpy.context.view_layer.objects.active = parent
        m = parent.modifiers.new(f"C_{child.name}", 'BOOLEAN')
        m.operation='DIFFERENCE'; m.object=child; m.solver='EXACT'
        bpy.ops.object.modifier_apply(modifier=f"C_{child.name}")
        bpy.data.objects.remove(child, do_unlink=True)


def flatten_plateau(body, plateau_r, z_plat=None):
    """Bake transform, cut the narrow tip off at the height where the body is still
    >= plateau_r in radius (or at an explicit z_plat), and cap it -> a flat plateau
    wide enough to host the whole connector base. Returns the plateau Z (Blender Z-up)."""
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    if z_plat is None:
        co = np.array([(v.co.x, v.co.y, v.co.z) for v in body.data.vertices])
        r = np.hypot(co[:, 0], co[:, 1]); z = co[:, 2]
        zc = z[r >= plateau_r]
        z_plat = float(zc.max()) if len(zc) else float(z.max())
    bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.bisect(plane_co=(0, 0, z_plat), plane_no=(0, 0, 1),
                        clear_inner=False, clear_outer=True, use_fill=True)
    bpy.ops.object.mode_set(mode='OBJECT')
    return z_plat


def fillet_junction(body, z_plat, voxel):
    """Voxel-remesh for uniform topology, then relax the plateau-rim band so the
    connector flares into the body (dome/ridge tops stay crisp for the mate)."""
    body.data.remesh_voxel_size = voxel
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    bpy.ops.object.voxel_remesh()
    me = body.data; bm = bmesh.new(); bm.from_mesh(me)
    rim = RIDGE_OD / 2
    for v in bm.verts:
        rr = (v.co.x ** 2 + v.co.y ** 2) ** 0.5
        v.select = (rim - 1.4 <= rr <= rim + 6.0) and (z_plat - 6.5 <= v.co.z <= z_plat + 1.6)
    for e in bm.edges: e.select = all(x.select for x in e.verts)
    for f in bm.faces: f.select = all(x.select for x in f.verts)
    bm.to_mesh(me); bm.free()
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.vertices_smooth(factor=0.5, repeat=30)
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.fill_holes(sides=12)              # close small remesh holes (not the socket)
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.object.mode_set(mode='OBJECT')


def _bake(obj):
    bpy.ops.object.select_all(action='DESELECT'); obj.select_set(True)
    bpy.context.view_layer.objects.active = obj
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)


def _radius_at(obj, z0, half=0.6):
    """Max body radius within +-half of height z0 (Blender Z-up; bake first)."""
    co = np.array([(v.co.x, v.co.y, v.co.z) for v in obj.data.vertices])
    r = np.hypot(co[:, 0], co[:, 1]); z = co[:, 2]
    sel = np.abs(z - z0) <= half
    return float(r[sel].max()) if sel.any() else float(r.max())


def add_seamless_crown(body, floor_z):
    """No flat shelf: cut the body just under the connector, then UNION a domed crown -- a
    revolved sor meridian that leaves the body wall tangentially and domes up to the ridge
    base at floor_z. For a narrow apex (MOVE's spike) the cut drops until the body is wide
    enough, so the crown FLARES the spike out instead of perching on it."""
    import sor
    plateau_r = RIDGE_OD / 2 + SEAT_MARGIN
    _bake(body)
    # Cut at the HIGHEST height where the body is still >= plateau_r, so the crown spans only a
    # small radius band and the body's OWN rounding becomes the crown. For a narrow apex (MOVE's
    # spike) that height is low, so the crown instead FLARES the spike out up to the connector.
    co = np.array([(v.co.x, v.co.y, v.co.z) for v in body.data.vertices])
    rr = np.hypot(co[:, 0], co[:, 1]); zz = co[:, 2]
    hi = zz[rr >= plateau_r]
    z_plat = min(float(hi.max()) if len(hi) else float(zz.max()), floor_z - 0.8)
    R_blend = max(_radius_at(body, z_plat), plateau_r)
    R_lo    = max(_radius_at(body, z_plat - 2.0), R_blend + 0.5)
    slope   = 2.0 / (R_blend - R_lo)                   # body wall dz/dr (negative)
    flatten_plateau(body, plateau_r, z_plat)           # cut top off; flat cap becomes internal
    md = sor.Meridian(R_blend, z_plat)
    md.hermite_to(RIDGE_OD / 2, floor_z, m0=slope, m1=0.0, n=40)
    prof = [(0.0, z_plat - 0.8), (R_blend, z_plat - 0.8)] + md.pts + [(0.0, floor_z)]
    crown = sor.revolve(prof, "Crown", seg=2 * DOME_SEGS)
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    mod = body.modifiers.new("U_Crown", 'BOOLEAN')
    mod.operation = 'UNION'; mod.object = crown; mod.solver = 'EXACT'
    bpy.ops.object.modifier_apply(modifier="U_Crown")
    bpy.data.objects.remove(crown, do_unlink=True)
    print("  seamless crown: base O%.1f @z%.1f -> ridge base z%.1f (rise %.1f, slope %.2f)"
          % (2 * R_blend, z_plat, floor_z, floor_z - z_plat, slope))
    return z_plat


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

    if args.seamless:
        # Domed crown grown out of the body -- no flat shelf, no sharp rim.
        floor_z = (args.height - DOME_HEIGHT) if args.height is not None \
            else (max(v.co.z for v in body.data.vertices) - 1.5)
        z_plat = add_seamless_crown(body, floor_z)
        add_peg(body, floor_z)
        if args.piece != "EAT":
            add_socket(body); print("  socket carved at z = 0")
        fillet_junction(body, z_plat, args.voxel)   # voxel remesh LAST: blends crown + re-knits socket
    elif args.fuse:
        # Seat the connector on a flat plateau and fillet it in (grown-in look).
        plateau_r = max(RIDGE_OD / 2 + 2.0, 8.0)
        z_plat = flatten_plateau(body, plateau_r, args.z)
        print(f"  fuse: plateau at z = {z_plat:.2f} mm (O{2*plateau_r:.0f} seat)")
        add_peg(body, z_plat)
        # cut the socket BEFORE the fuse remesh, so the remesh re-knits peg+body+
        # socket into ONE clean watertight manifold (it softens the socket by ~half
        # a voxel, which only makes the mate more forgiving)
        if args.piece != "EAT":
            add_socket(body); print(f"  socket carved at z = 0")
        fillet_junction(body, z_plat, args.voxel)
    else:
        # Truncate the narrow tip to a flat plateau wide enough for the WHOLE connector
        # base, then seat the peg flush on it. Without this the peg lands on the apex and
        # floats over a too-thin tip (MOVE's spike is ~7mm too narrow). Per-piece: each
        # truncates only what it needs (EAT/GROW ~1-2mm, MOVE ~8mm).
        plateau_r = RIDGE_OD / 2 + SEAT_MARGIN
        z_plat = flatten_plateau(body, plateau_r, args.z)
        print(f"  truncated to plateau z = {z_plat:.2f} mm (O{2*plateau_r:.0f} seat); peg seated flush")
        add_peg(body, z_plat)
        # FOOD/MOVE/GROW get the bottom socket; EAT doesn't (food never goes UNDER EAT)
        if args.piece != "EAT":
            add_socket(body)
            print(f"  socket carved at z = 0")

    # Merge boolean-induced coincident verts -> watertight 2-manifold
    bpy.ops.object.select_all(action='DESELECT')
    body.select_set(True); bpy.context.view_layer.objects.active = body
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.remove_doubles(threshold=1e-4)
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.object.mode_set(mode='OBJECT')

    out_path = Path(args.out) if args.out else in_path.parent / f"{in_path.stem}_connected.obj"
    bpy.ops.object.select_all(action='DESELECT')
    body.select_set(True); bpy.context.view_layer.objects.active = body
    bpy.ops.wm.obj_export(filepath=str(out_path), export_selected_objects=True,
                          forward_axis='NEGATIVE_Z', up_axis='Y', export_materials=False)
    print(f"\n  → {out_path}")


if __name__ == "__main__":
    main()
