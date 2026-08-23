"""Gather all 3D-printable Organism STLs into a single directory.

  3D pieces : out/{EAT,MOVE,GROW}_sculpt_graft.stl  -> {EAT,MOVE,GROW}.stl   (copy, canonical)
  FOOD      : renders/food/FOOD_{nosnap,snap}.stl    -> FOOD_{slip,snap}.stl  (copy, proven current)
  2D disks  : out/{EAT,MOVE,GROW}_mindisk.stl        -> {EAT,MOVE,GROW}_2d.stl (copy, proven current)
"""
import os, shutil, trimesh

HERE = "/home/youdonotexist/code/organism/pieces"
DST  = os.path.join(HERE, "stl")
os.makedirs(DST, exist_ok=True)

# (dst name, source path, mode)  mode: "copy" byte-for-byte | "obj2stl" convert (see caveat below)
#
# The pieces are COPIED from the STL that flatten_bottom.py writes alongside each OBJ, not
# converted from the OBJ. Converting is lossy here: Blender's OBJ carries split vertex normals,
# so trimesh builds a separate vertex per (v, vn) pair, fragmenting face adjacency. Round-tripping
# MOVE that way manufactured 9 phantom "sliver" bodies and reported the piece non-watertight,
# even though the mesh itself is a single watertight solid. Copying the Blender STL is exact.
JOBS = [
    ("EAT.stl",       f"{HERE}/out/EAT_sculpt_graft.stl",   "copy"),
    ("MOVE.stl",      f"{HERE}/out/MOVE_sculpt_graft.stl",  "copy"),
    ("GROW.stl",      f"{HERE}/out/GROW_sculpt_graft.stl",  "copy"),
    ("FOOD_slip.stl", f"{HERE}/renders/food/FOOD_nosnap.stl", "copy"),
    ("FOOD_snap.stl", f"{HERE}/renders/food/FOOD_snap.stl",   "copy"),
    ("EAT_2d.stl",    f"{HERE}/out/EAT_mindisk.stl",        "copy"),
    ("MOVE_2d.stl",   f"{HERE}/out/MOVE_mindisk.stl",       "copy"),
    ("GROW_2d.stl",   f"{HERE}/out/GROW_mindisk.stl",       "copy"),
]

for name, src, mode in JOBS:
    assert os.path.exists(src), f"MISSING SOURCE: {src}"
    dst = os.path.join(DST, name)
    if mode == "obj2stl":
        m = trimesh.load(src, process=False, force='mesh')   # process=False -> preserve geometry exactly
        m.export(dst)                                         # .stl inferred from extension (binary)
    else:
        shutil.copy2(src, dst)

# ---- verification pass over the FINAL files in the directory ----
# NOTE: load with process=True (merge coincident verts) or the numbers are meaningless. An STL
# is a triangle soup with no shared vertices, so process=False makes EVERY edge look like a
# boundary edge and every mesh look non-watertight. That artifact is what hid a real problem
# here for a while -- see the JOBS comment above.
print(f"{'file':16s} {'tris':>8s} {'bodies':>7s} {'watertt':>8s} {'openEdg':>8s} "
      f"{'X':>6s} {'Y':>6s} {'Z':>6s}  src")
print("-" * 100)
bad = []
for name, src, mode in JOBS:
    m = trimesh.load(os.path.join(DST, name), force='mesh')
    e = m.extents
    bodies = len(m.split(only_watertight=False))
    # open (boundary) edges = edges used by exactly one face
    open_edges = int((trimesh.grouping.group_rows(m.edges_sorted, require_count=1).shape[0]))
    if not m.is_watertight or open_edges:
        bad.append(name)
    print(f"{name:16s} {len(m.faces):8d} {bodies:7d} {str(m.is_watertight):>8s} {open_edges:8d} "
          f"{e[0]:6.1f} {e[1]:6.1f} {e[2]:6.1f}  {os.path.basename(src)}")
if bad:
    print(f"\n!! NOT WATERTIGHT: {', '.join(bad)} -- slicers will prompt to repair these")
print(f"\nwrote {len(JOBS)} STLs -> {DST}")
