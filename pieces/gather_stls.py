"""Gather all 3D-printable Organism STLs into a single directory.

  3D pieces : out/{EAT,MOVE,GROW}_sculpt_graft.obj  -> {EAT,MOVE,GROW}.stl   (regen from current OBJ)
  FOOD      : renders/food/FOOD_{nosnap,snap}.stl    -> FOOD_{slip,snap}.stl  (copy, proven current)
  2D disks  : out/{EAT,MOVE,GROW}_mindisk.stl        -> {EAT,MOVE,GROW}_2d.stl (copy, proven current)
"""
import os, shutil, trimesh

HERE = "/home/youdonotexist/code/organism/pieces"
DST  = os.path.join(HERE, "stl")
os.makedirs(DST, exist_ok=True)

# (dst name, source path, mode)  mode: "obj2stl" faithful convert | "copy" byte-for-byte
JOBS = [
    ("EAT.stl",       f"{HERE}/out/EAT_sculpt_graft.obj",   "obj2stl"),
    ("MOVE.stl",      f"{HERE}/out/MOVE_sculpt_graft.obj",  "obj2stl"),
    ("GROW.stl",      f"{HERE}/out/GROW_sculpt_graft.obj",  "obj2stl"),
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
print(f"{'file':16s} {'tris':>8s} {'watertt':>8s} {'openEdg':>8s} "
      f"{'X':>6s} {'Y':>6s} {'Z':>6s}  src")
print("-" * 92)
for name, src, mode in JOBS:
    m = trimesh.load(os.path.join(DST, name), process=False, force='mesh')
    e = m.extents
    # open (boundary) edges = edges used by exactly one face
    open_edges = int((trimesh.grouping.group_rows(m.edges_sorted, require_count=1).shape[0]))
    print(f"{name:16s} {len(m.faces):8d} {str(m.is_watertight):>8s} {open_edges:8d} "
          f"{e[0]:6.1f} {e[1]:6.1f} {e[2]:6.1f}  {os.path.basename(src)}")
print(f"\nwrote {len(JOBS)} STLs -> {DST}")
