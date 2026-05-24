"""sor.py - Solids Of Revolution: the shared vocabulary for organism's round parts (food,
the universal connector, future revolved pieces).

PRINCIPLES (everything here is written in these terms)
  1. A part is a MERIDIAN: an ordered list of (radius, height) points, revolved about +z.
  2. Build meridians by chaining G1 PRIMITIVES; each starts at the previous point and
     matches its tangent: line / parabola / bead(rounded rim) / bezier(tangent rim) /
     hermite(smooth ramp) / dish(smoothstep). -> continuity comes for free.
  3. CONTINUITY is a checked invariant: Meridian.hard_edges() flags any tangent break.
  4. The CONNECTOR is one parametric spec (peg+socket); fit is a single number `gap`
     (+ clearance = slip, - = interference = snap).  [lives in graft_connector.py]
  5. MEASURE, don't eyeball: nest_offset() (telescoping), wall checks, manifold (print).
  6. Two render paths: preview() here (CPU, matplotlib) for iteration; Blender for final.
  7. Build the WHOLE part as ONE meridian -> a single revolve is watertight. Booleans
     (EXACT) shatter the mesh; one revolve never does. This is what makes parts printable.

The pure-Python half (Meridian, checks, nesting, preview) runs in the repo .venv.
The Blender half (revolve, export) is imported lazily and needs `bpy`.
"""
from __future__ import annotations
import math

# ---------------------------------------------------------------- profile algebra
class Meridian:
    """An (r,z) cross-section, built left-to-right with tangent-matched primitives."""
    def __init__(self, r0=0.0, z0=0.0):
        self.pts = [(float(r0), float(z0))]

    # -- primitives (each appends points ending at the new location) --
    def line_to(self, r, z):
        self.pts.append((float(r), float(z))); return self

    def parabola_to(self, r1, z1, slope0=0.0, n=48):
        """Parabola from the current point to (r1,z1) leaving at slope0 (dz/dr)."""
        r0, z0 = self.pts[-1]
        # z = z0 + slope0*(r-r0) + c*(r-r0)^2 ; solve c from the endpoint
        dr = r1 - r0
        c = (z1 - z0 - slope0 * dr) / (dr * dr) if dr else 0.0
        for i in range(1, n + 1):
            r = r0 + dr * i / n
            self.pts.append((r, z0 + slope0 * (r - r0) + c * (r - r0) ** 2))
        return self

    def dish_to(self, r1, z1, n=48):
        """Smoothstep ramp (flat tangent at both ends) -> a dished/blended segment."""
        r0, z0 = self.pts[-1]
        for i in range(1, n + 1):
            u = i / n; s = u * u * (3 - 2 * u)
            self.pts.append((r0 + (r1 - r0) * u, z0 + (z1 - z0) * s))
        return self

    def hermite_to(self, r1, z1, m0, m1, n=24):
        """Cubic Hermite from current point (slope m0) to (r1,z1) (slope m1). G1 both ends."""
        r0, z0 = self.pts[-1]; dr = r1 - r0
        for i in range(1, n + 1):
            t = i / n
            h00 = 2*t**3 - 3*t**2 + 1; h10 = t**3 - 2*t**2 + t
            h01 = -2*t**3 + 3*t**2;    h11 = t**3 - t**2
            self.pts.append((r0 + dr * t,
                             h00*z0 + h10*dr*m0 + h01*z1 + h11*dr*m1))
        return self

    def bezier_to(self, r1, z1, t0, t1, L, n=44):
        """Cubic Bezier rim from current point to (r1,z1), tangent to direction t0 (out)
        and arriving along t1 (out) -> rounds a rim tangent to BOTH adjoining surfaces."""
        P0 = self.pts[-1]; P3 = (r1, z1)
        u0 = _unit(t0); u1 = _unit(t1)
        P1 = (P0[0] + L*u0[0], P0[1] + L*u0[1])
        P2 = (P3[0] + L*u1[0], P3[1] + L*u1[1])
        for i in range(1, n):
            t = i / n; m = 1 - t
            self.pts.append((m**3*P0[0] + 3*m*m*t*P1[0] + 3*m*t*t*P2[0] + t**3*P3[0],
                             m**3*P0[1] + 3*m*m*t*P1[1] + 3*m*t*t*P2[1] + t**3*P3[1]))
        self.pts.append(P3); return self

    def points(self): return self.pts

    # -- invariant 3: continuity --
    def hard_edges(self, tol_deg=12.0):
        out = []
        for i in range(1, len(self.pts) - 1):
            ax, ay = self.pts[i][0]-self.pts[i-1][0], self.pts[i][1]-self.pts[i-1][1]
            bx, by = self.pts[i+1][0]-self.pts[i][0], self.pts[i+1][1]-self.pts[i][1]
            la, lb = math.hypot(ax, ay), math.hypot(bx, by)
            if la < 1e-9 or lb < 1e-9: continue
            ang = math.degrees(math.acos(max(-1.0, min(1.0, (ax*bx+ay*by)/(la*lb)))))
            if ang > tol_deg: out.append((i, self.pts[i], ang))
        return out


def _unit(v):
    n = math.hypot(*v); return (v[0]/n, v[1]/n) if n else (0.0, 0.0)


def _dz(f, x, h=1e-4):
    return (f(x + h) - f(x - h)) / (2 * h)


# ---------------------------------------------------------- connector as profile (principle 7)
# The universal peg/socket, expressed as meridian features so the WHOLE part is one revolve
# (no booleans -> watertight). Shapes match graft_connector.py (build_parabolic_dome / _ridge_ring).
def _dome(r_base, height, z_base, n=22):
    """Parabolic dome meridian: base (r_base, z_base) -> apex (0, z_base+height).
    z = z_base + height*(1 - (r/r_base)^2)  (same parabola as the grafted connector)."""
    return [(r_base * (1 - i / n), z_base + height * (1 - (1 - i / n) ** 2)) for i in range(n + 1)]


def _ridge(ir, orr, height, peak_w, z_base, n=26):
    """Peaked annular ridge meridian: (ir, z_base) -> flat peak -> (orr, z_base)."""
    cf = peak_w / (2 * (orr - ir)) if orr > ir else 0.0
    ef = max(1 - cf, 1e-6)
    out = []
    for i in range(n + 1):
        u = i / n; r = ir + (orr - ir) * u; uc = abs(2 * u - 1)
        bump = 1.0 if uc < cf else max(0.0, 1 - ((uc - cf) / ef) ** 2)
        out.append((r, z_base + height * bump))
    return out


def food(R=13.0, flare_up=2.5, wall=2.5, gap=0.2, dome_r=1.9, dome_h=4.3,
         ridge_ir=4.15, ridge_or=6.4, ridge_h=2.75, peak_w=2.0, seat_pad=0.6, nb=48):
    """The whole FOOD as ONE meridian: socket cavity -> seat -> underside -> rim -> top ->
    peg ridge -> peg dome. A single revolve of this is watertight (no booleans). The socket
    is the connector oversized by `gap` (+ slip / - snap). Returns (points, meta)."""
    socket_ceiling = dome_h + gap                          # socket cavity depth (from z=0)
    floor_z = socket_ceiling + wall                        # solid floor under the peg
    peg_tip = floor_z + dome_h
    R_seat = ridge_or + gap + seat_pad
    z_top = lambda r: floor_z + flare_up * ((r - ridge_or) / (R - ridge_or)) ** 2
    rim_b = z_top(R) - wall
    z_bot = lambda r: rim_b * ((r - R_seat) / (R - R_seat)) ** 2
    s_dr, s_dh = dome_r + gap, dome_h + gap                # socket dome cavity
    s_ir, s_or, s_rh = ridge_ir - gap, ridge_or + gap, ridge_h + gap   # socket ridge cavity

    P = _dome(s_dr, s_dh, 0.0)[::-1]                        # socket dome: apex -> mouth
    P.append((s_ir, 0.0))                                   # flat -> socket ridge inner
    P += _ridge(s_ir, s_or, s_rh, peak_w, 0.0)[1:]         # socket ridge cavity
    P.append((R_seat, 0.0))                                 # flat seat
    P += [(R_seat + (R - R_seat) * i / nb, z_bot(R_seat + (R - R_seat) * i / nb)) for i in range(1, nb + 1)]
    rim = Meridian(); rim.pts = [P[-1]]                     # rounded rim, tangent to both surfaces
    rim.bezier_to(R, z_top(R), (1, _dz(z_bot, R)), (1, _dz(z_top, R)), wall * 0.9, n=96)
    P += rim.pts[1:]
    P += [(R - (R - ridge_or) * i / nb, z_top(R - (R - ridge_or) * i / nb)) for i in range(1, nb + 1)]
    P += _ridge(ridge_ir, ridge_or, ridge_h, peak_w, floor_z)[::-1][1:]   # peg ridge: OR -> IR
    P.append((dome_r, floor_z))                            # flat floor -> dome base
    P += _dome(dome_r, dome_h, floor_z)[1:]                # peg dome: base -> apex
    meta = dict(peg_tip=peg_tip, socket_ceiling=socket_ceiling, floor_z=floor_z,
                ridge_or=ridge_or, socket_or=s_or, max_r=R)
    return P, meta


# ---------------------------------------------------------------- invariant 5: nesting
def nest_offset(prof, peg_tip, socket_ceiling, core_r=6.6):
    """Vertical offset D at which two identical coaxial parts rest, and nested fraction.
    The tightest spot wins: outside the connector it's the body's max thickness; in the
    center (r < core_r) the SOCKET carves the body, so the connector (peg_tip - socket_ceiling)
    governs instead. nested_fraction = 1 - D/H. Matches the mesh-accurate stack_food.py."""
    import numpy as np
    P = np.array(prof); r, z = P[:, 0], P[:, 1]
    NB = 80; edges = np.linspace(0, r.max() + 1e-9, NB + 1)
    bi = np.clip(np.digitize(r, edges) - 1, 0, NB - 1)
    body = max((z[bi == k].max() - z[bi == k].min()
                for k in range(NB) if (bi == k).any() and (edges[k] + edges[k+1]) / 2 >= core_r),
               default=0.0)
    D = max(body, peg_tip - socket_ceiling)                 # connector core vs body wall
    outside = z[r >= core_r]
    lowest = min(socket_ceiling, float(outside.min()) if outside.size else socket_ceiling)
    H = max(float(z.max()), peg_tip) - lowest               # lowest solid (socket) -> highest (peg)
    return D, H, 1 - D / H


# ---------------------------------------------------------------- invariant 6: CPU preview
def preview(prof, path, title="", connector=None):
    """Quick matplotlib cross-section of a meridian (mirrored). CPU, no Blender."""
    import numpy as np, matplotlib
    matplotlib.use("Agg"); import matplotlib.pyplot as plt
    P = np.array(prof)
    fig, ax = plt.subplots(figsize=(7, 5))
    ax.plot(P[:, 0], P[:, 1], "-", color="#a85a12", lw=2)
    ax.plot(-P[:, 0], P[:, 1], "-", color="#a85a12", lw=2)
    if connector:
        ax.plot([0, 0], [connector["socket_ceiling"], connector["peg_tip"]], color="#b52b27", lw=3, alpha=.6)
    ax.set_aspect("equal"); ax.set_xlabel("radius (mm)"); ax.set_ylabel("height (mm)")
    ax.set_title(title, weight="bold")
    fig.tight_layout(); fig.savefig(path, dpi=130); print("wrote", path)


# ---------------------------------------------------------------- Blender half (needs bpy)
def revolve(prof, name="part", seg=120):
    """Revolve a meridian into a solid mesh object (Blender). One revolve = watertight."""
    import bpy, bmesh, math as _m
    bm = bmesh.new()
    vs = [bm.verts.new((max(r, 0.0), 0.0, z)) for (r, z) in prof]
    for a, b in zip(vs, vs[1:]): bm.edges.new((a, b))
    me = bpy.data.meshes.new(name); bm.to_mesh(me); bm.free()
    o = bpy.data.objects.new(name, me); bpy.context.collection.objects.link(o)
    bpy.ops.object.select_all(action='DESELECT')    # else Edit-mode spin hits other meshes (multi-object edit)
    bpy.context.view_layer.objects.active = o; o.select_set(True)
    bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.spin(steps=seg, angle=_m.radians(360), axis=(0, 0, 1), center=(0, 0, 0))
    bpy.ops.mesh.select_all(action='SELECT')        # spin leaves only new geom selected
    bpy.ops.mesh.remove_doubles(threshold=1e-4)     # merge the seam + collapse the poles
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.object.mode_set(mode='OBJECT')
    return o


def manifold_report(obj):
    """Count non-manifold edges (0 = watertight, printable). Blender."""
    import bmesh
    bm = bmesh.new(); bm.from_mesh(obj.data)
    n = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    return n


def export(obj, path_noext, obj_axes=('NEGATIVE_Z', 'Y')):
    """Export OBJ (axes for the scene) + STL (print, mm). Blender."""
    import bpy, os
    os.makedirs(os.path.dirname(path_noext), exist_ok=True)
    bpy.ops.object.select_all(action='DESELECT'); obj.select_set(True)
    bpy.context.view_layer.objects.active = obj
    bpy.ops.wm.obj_export(filepath=path_noext + ".obj", export_selected_objects=True,
                          forward_axis=obj_axes[0], up_axis=obj_axes[1], export_materials=False)
    try:
        bpy.ops.wm.stl_export(filepath=path_noext + ".stl", export_selected_objects=True)
    except Exception:
        bpy.ops.export_mesh.stl(filepath=path_noext + ".stl", use_selection=True)
    print("wrote", path_noext + ".obj /.stl")
