"""FOOD = parabolic flare with a ROUNDED rim, plus a small FLAT underside around the
connector so it actually seats on a piece's flat plateau (the connector's seating
face). The visible top is a clean parabola flaring up; the connector is grafted into
the middle afterwards. Same connector spec as the pieces, so it stacks.

Profile (surface of revolution), z=0 at the flat underside:
  - seat: FLAT (z=0) for r <= R_BEAR              (seats on the piece / connector face).
  - wall: quarter-ellipse flaring up; turns VERTICAL at the rim (its widest shoulder),
          so the rim inherits an UPWARD direction, not an outward one.
  - rim:  quarter-circle roll-over; starts vertical, curls UP & INWARD over the top.
  - bowl: from the rim apex it dishes DOWN to a flat floor at z = FLOOR_Z (radius R_FLOOR);
          the connector grafts onto that floor, recessed "at the bottom of the bowl".
The whole profile is G1 (tangent-continuous): flat -> wall -> rim -> bowl, no lip.

Run:  blender --background --python build_food.py
Tune via env: FOOD_R (shoulder radius), FOOD_RBEAR (flat seating radius),
              FOOD_FB (shoulder height / bowl depth), FOOD_TR (rim bead thickness),
              FOOD_FLOOR (bowl-floor height = center thickness), FOOD_RFLOOR (floor radius).
"""
import bpy, bmesh, math, sys, os, json
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import graft_connector as gc

# Connector fit: FOOD_GAP is the socket oversize. Positive = clearance (slip, slides on/off
# easily); negative = interference (a snap that holds). Default keeps the slip fit.
gc.SOCKET_GAP = float(os.environ.get("FOOD_GAP", str(gc.SOCKET_GAP)))

R       = float(os.environ.get("FOOD_R",    "13.0"))    # shoulder radius (max Ø ~ 2R)
FLARE_B = float(os.environ.get("FOOD_FB",   "10.0"))    # shoulder height = bowl depth (open parabola)
W_MIN   = float(os.environ.get("FOOD_WALL",  "2.5"))    # INVARIANT injection-molding wall (uniform, with buffer)
PEG_SINK = 0.5                                          # weld the peg this far into the floor
T_W     = W_MIN                                         # shell wall = the invariant
# Boss sized so the socket has a W_MIN wall all round: side wall + floor under the peg.
_sock_or = gc.RIDGE_OD / 2.0 + gc.SOCKET_GAP           # socket cavity outer radius (~6.6)
_sock_h  = gc.DOME_HEIGHT + gc.SOCKET_GAP              # socket cavity depth from z=0 (~4.5)
R_BEAR  = _sock_or + W_MIN                              # foot / boss radius (W_MIN wall around socket)
FLOOR_Z = _sock_h + PEG_SINK + W_MIN                    # boss height (W_MIN floor under the peg)
SEG = 120; NB = 48; NR = 44; NF = 16; NW = 16
OUT = os.environ.get("FOOD_OUT",
        os.path.join(os.path.dirname(os.path.abspath(__file__)), "renders/food/FOOD_new.obj"))

bpy.ops.wm.read_factory_settings(use_empty=True)

rad = T_W / 2.0                               # rounded-rim bead radius
FLARE_UP = float(os.environ.get("FOOD_FLAREUP", "2.5"))   # how far the top flares UP at the rim

# Profile (OUTER, visible): a shallow parabola flaring UPWARDS out of the connector. Both
# surfaces curve up to a rounded rim (a meniscus). This outer profile stays smooth/G1; the
# thick interior is hollowed afterwards by a separate cavity cut from below (see CAVITY) so
# the moulded wall stays ~T_W. Underside z_bot is kept so stacking still nests on the connector.
BOT_RISE = FLOOR_Z + FLARE_UP - T_W
def z_top(r): return FLOOR_Z + FLARE_UP * (r / R) ** 2
def z_bot(r): return BOT_RISE * (r / R) ** 2
def under(r): return z_top(r) - T_W
def _slope(f, x, h=1e-4): return (f(x + h) - f(x - h)) / (2.0 * h)
def _unit(v): n = math.hypot(*v); return (v[0] / n, v[1] / n)

prof = [(0.0, 0.0)]
for i in range(1, NB + 1):                    # underside: flares up, center -> rim
    r = R * i / NB; prof.append((r, z_bot(r)))
P0, P3 = (R, z_bot(R)), (R, z_top(R))         # rounded rim: cubic Bezier tangent to BOTH curves
ub, ut = _unit((1.0, _slope(z_bot, R))), _unit((1.0, _slope(z_top, R)))
L = T_W * 0.9
P1 = (P0[0] + L * ub[0], P0[1] + L * ub[1])
P2 = (P3[0] + L * ut[0], P3[1] + L * ut[1])
for i in range(1, NR):
    t = i / NR; mt = 1.0 - t
    prof.append((mt**3*P0[0] + 3*mt*mt*t*P1[0] + 3*mt*t*t*P2[0] + t**3*P3[0],
                 mt**3*P0[1] + 3*mt*mt*t*P1[1] + 3*mt*t*t*P2[1] + t**3*P3[1]))
for i in range(1, NB + 1):                    # top: flares up, rim -> center (0, FLOOR_Z)
    r = R * (1.0 - i / NB); prof.append((r, z_top(r)))
maxr = max(p[0] for p in prof)

# CONTINUITY invariant: flag any hard edge (tangent break > tol) between profile segments.
def _tangent_breaks(p, tol=12.0):
    out = []
    for i in range(1, len(p) - 1):
        ax, ay = p[i][0] - p[i-1][0], p[i][1] - p[i-1][1]
        bx, by = p[i+1][0] - p[i][0], p[i+1][1] - p[i][1]
        la, lb = math.hypot(ax, ay), math.hypot(bx, by)
        if la < 1e-9 or lb < 1e-9: continue
        out.append((i, p[i], math.degrees(math.acos(max(-1.0, min(1.0, (ax*bx + ay*by) / (la*lb)))))))
    return [b for b in out if b[2] > tol]
_breaks = _tangent_breaks(prof)
print("CONTINUITY: " + ("G1 OK (no tangent break > 12°)" if not _breaks else
      "%d hard edge(s): " % len(_breaks) +
      ", ".join("(r=%.1f,z=%.1f)=%.0f°" % (q[0], q[1], a) for _, q, a in _breaks[:5])))

bm = bmesh.new()
vs = [bm.verts.new((max(r, 0.0), 0.0, z)) for (r, z) in prof]
for a, b in zip(vs, vs[1:]):
    bm.edges.new((a, b))
me = bpy.data.meshes.new("FOOD"); bm.to_mesh(me); bm.free()
body = bpy.data.objects.new("FOOD", me); bpy.context.collection.objects.link(body)
bpy.context.view_layer.objects.active = body; body.select_set(True)
bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.spin(steps=SEG, angle=math.radians(360), axis=(0, 0, 1), center=(0, 0, 0))
bpy.ops.mesh.remove_doubles(threshold=1e-4)
bpy.ops.mesh.normals_make_consistent(inside=False)
bpy.ops.object.mode_set(mode='OBJECT')

# graft the universal connector into the middle (after the flare)
bpy.context.view_layer.objects.active = body; body.select_set(True)
gc.add_peg(body, FLOOR_Z - PEG_SINK)
gc.add_socket(body)

bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
bpy.context.view_layer.objects.active = body
bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.remove_doubles(threshold=1e-4)
bpy.ops.mesh.normals_make_consistent(inside=False)
bpy.ops.object.mode_set(mode='OBJECT')

os.makedirs(os.path.dirname(OUT), exist_ok=True)
bpy.ops.object.select_all(action='DESELECT'); body.select_set(True); bpy.context.view_layer.objects.active = body
bpy.ops.wm.obj_export(filepath=OUT, export_selected_objects=True,
                      forward_axis='NEGATIVE_Z', up_axis='Y', export_materials=False)
zr = [v.co.z for v in body.data.vertices]
# sidecar for the stacking-metric tool: exact meridian + connector envelope (artifact-free)
with open(os.path.splitext(OUT)[0] + ".profile.json", "w") as f:
    json.dump({"prof": prof, "floor_z": FLOOR_Z, "wall": T_W,
               "peg_tip": (FLOOR_Z - PEG_SINK) + gc.DOME_HEIGHT,   # highest solid at the axis
               "ridge_or": gc.RIDGE_OD / 2.0,                      # peg ridge outer radius
               "socket_or": gc.RIDGE_OD / 2.0 + gc.SOCKET_GAP,     # socket cavity outer radius
               "socket_ceiling": gc.DOME_HEIGHT + gc.SOCKET_GAP},  # socket cavity depth from z=0
              f)
print("FOOD_new: maxØ=%.1f, center-thick %.1f, flare-up %.1f, wall %.1f, height=%.1f mm"
      % (2 * maxr, FLOOR_Z, FLARE_UP, T_W, max(zr) - min(zr)))
print("wrote", OUT)
