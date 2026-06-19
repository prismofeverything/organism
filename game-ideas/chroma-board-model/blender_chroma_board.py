#!/usr/bin/env python3
"""
Chroma board — Blender visualization / import helper.

Imports an STL produced by chroma_board.scad, gives it a translucent backlit
look, and (headless) renders a preview PNG. Use it to eyeball the model and to
mock up the backlight glow before printing.

  # render a preview headless:
  blender -b -P blender_chroma_board.py -- chroma_board.stl preview.png

  # or just open it in the GUI with the model loaded:
  blender -P blender_chroma_board.py -- chroma_board.stl

The render fakes the backlight with an emissive plane sitting under the board
(light shining up through each 12.7 mm hole), so the preview reads like the
real lit rig rather than a grey print.
"""
import bpy, sys, os, math

# ---- args after "--" ----
argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
stl_path = argv[0] if argv else "chroma_board.stl"
out_png  = argv[1] if len(argv) > 1 else None
stl_path = os.path.abspath(stl_path)

# ---- clean slate ----
bpy.ops.wm.read_factory_settings(use_empty=True)

# ---- import STL (Blender 4.x fast importer, with fallback) ----
try:
    bpy.ops.wm.stl_import(filepath=stl_path)
except Exception:
    bpy.ops.import_mesh.stl(filepath=stl_path)   # legacy
obj = bpy.context.selected_objects[0]
obj.name = "ChromaBoard"

# center on origin
bpy.ops.object.origin_set(type="ORIGIN_GEOMETRY", center="BOUNDS")
obj.location = (0, 0, 0)
dim = obj.dimensions
span = max(dim.x, dim.y)

# ---- board material: matte light-grey PLA ----
mat = bpy.data.materials.new("PLA")
mat.use_nodes = True
bsdf = mat.node_tree.nodes["Principled BSDF"]
bsdf.inputs["Base Color"].default_value = (0.82, 0.82, 0.85, 1)
bsdf.inputs["Roughness"].default_value = 0.7
obj.data.materials.append(mat)

# ---- emissive backlight plane under the board ----
bpy.ops.mesh.primitive_plane_add(size=span * 1.3, location=(0, 0, -1))
glow = bpy.context.active_object
gm = bpy.data.materials.new("Backlight")
gm.use_nodes = True
em = gm.node_tree.nodes.new("ShaderNodeEmission")
em.inputs["Strength"].default_value = 6.0
em.inputs["Color"].default_value = (1.0, 0.98, 0.95, 1)
gm.node_tree.links.new(
    em.outputs["Emission"],
    gm.node_tree.nodes["Material Output"].inputs["Surface"])
glow.data.materials.append(gm)

# ---- camera (3/4 top-down) ----
cam_data = bpy.data.cameras.new("Cam")
cam = bpy.data.objects.new("Cam", cam_data)
bpy.context.scene.collection.objects.link(cam)
cam.location = (span * 0.75, -span * 0.75, span * 1.05)
cam.rotation_euler = (math.radians(48), 0, math.radians(45))
bpy.context.scene.camera = cam

# ---- fill light ----
sun_data = bpy.data.lights.new("Sun", type="SUN")
sun_data.energy = 2.5
sun = bpy.data.objects.new("Sun", sun_data)
sun.rotation_euler = (math.radians(35), math.radians(20), 0)
bpy.context.scene.collection.objects.link(sun)

# ---- render settings ----
scn = bpy.context.scene
scn.render.engine = "BLENDER_EEVEE_NEXT" if "BLENDER_EEVEE_NEXT" in \
    [e.identifier for e in bpy.types.RenderSettings.bl_rna.properties["engine"].enum_items] \
    else "BLENDER_EEVEE"
scn.render.resolution_x = 1400
scn.render.resolution_y = 1400
scn.render.film_transparent = False
scn.world = bpy.data.worlds.new("W")
scn.world.use_nodes = True
scn.world.node_tree.nodes["Background"].inputs["Color"].default_value = (0.04, 0.04, 0.05, 1)

if out_png:
    scn.render.filepath = os.path.abspath(out_png)
    bpy.ops.render.render(write_still=True)
    print("WROTE", scn.render.filepath)
