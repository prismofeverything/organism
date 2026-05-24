"""Diagnose where the seamless graft loses the body. Prints vert counts at each stage."""
import bpy, sys, os, numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import graft_connector as gc, sor

bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.wm.obj_import(filepath="EAT.obj", forward_axis='NEGATIVE_Z', up_axis='Y')
body = [o for o in bpy.context.scene.objects if o.type == 'MESH'][0]
nv = lambda: len(body.data.vertices)
zr = lambda: (round(min(v.co.z for v in body.data.vertices), 1), round(max(v.co.z for v in body.data.vertices), 1))
print("STAGE import      :", nv(), "z", zr())

gc._bake(body)
floor_z = 50.6 - gc.DOME_HEIGHT
plateau_r = gc.RIDGE_OD/2 + gc.SEAT_MARGIN
co = np.array([(v.co.x, v.co.y, v.co.z) for v in body.data.vertices])
rr = np.hypot(co[:, 0], co[:, 1]); zz = co[:, 2]
hi = zz[rr >= plateau_r]; z_plat = min(float(hi.max()), floor_z - 0.8)
print("floor_z=%.1f plateau_r=%.1f z_plat=%.1f R@z_plat=%.1f" % (floor_z, plateau_r, z_plat, gc._radius_at(body, z_plat)))

gc.flatten_plateau(body, plateau_r, z_plat)
print("STAGE flatten     :", nv(), "z", zr())

R_blend = max(gc._radius_at(body, z_plat), plateau_r)
R_lo = max(gc._radius_at(body, z_plat - 2.0), R_blend + 0.5)
slope = 2.0 / (R_blend - R_lo)
md = sor.Meridian(R_blend, z_plat); md.hermite_to(gc.RIDGE_OD/2, floor_z, m0=slope, m1=0.0, n=40)
prof = [(0.0, z_plat - 0.8), (R_blend, z_plat - 0.8)] + md.pts + [(0.0, floor_z)]
crown = sor.revolve(prof, "Crown", seg=128)
print("STAGE crown built : crown verts =", len(crown.data.vertices), " body still =", nv())

bpy.ops.object.select_all(action='DESELECT'); body.select_set(True); bpy.context.view_layer.objects.active = body
m = body.modifiers.new("U", 'BOOLEAN'); m.operation = 'UNION'; m.object = crown; m.solver = 'EXACT'
bpy.ops.object.modifier_apply(modifier="U")
print("STAGE crown union :", nv(), "z", zr())
