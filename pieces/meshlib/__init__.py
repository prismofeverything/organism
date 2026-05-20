"""
meshlib — standalone height-field mesh generator for the ORGANISM pieces.

Pipeline (a DAG of pure, typed stages; see ../DECISIONS.md):

    SVG ─▶ domain ─▶ field ─▶ mesh2d ─▶ lift ─▶ solid ─▶ validate ─▶ export

Each piece is a height field z = H(x,y) over its 2D SVG region R, so the
top-down silhouette equals R exactly and the surface has no undercuts. The
height comes from a smooth interior field (elastic-membrane / torsion function)
remapped by a side-profile transfer function. No Blender required for the core.
"""
__version__ = "0.0.1"
