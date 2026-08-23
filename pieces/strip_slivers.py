"""Drop degenerate connected components ("slivers") from a plate STL, in place.

Kept as a safety net. The 9 MOVE "slivers" this was written for turned out NOT to be in the
mesh: out/MOVE_sculpt.obj is a single clean component, and the STL Blender exports is one
watertight solid. They were manufactured by reading the OBJ back through trimesh (split vertex
normals + ASCII rounding fragment face adjacency) -- see the note in gather_stls.py. Now that
gather_stls.py copies the Blender STL instead of converting the OBJ, this pass reports
"no slivers" on every current plate.

The test is still cheap insurance against a genuinely degenerate export: real pieces (and food)
are 70k+ faces and 10^4 mm^3, slivers are <= 4 faces and ~0 -- an astronomical gap -- so the
(faces < MIN_FACES AND |vol| < MIN_VOL) test can only ever remove junk, never a real solid.
Kept components are re-concatenated unchanged (verts/faces byte-for-byte).

Run:  python strip_slivers.py <plate.stl>
"""
import sys, os, trimesh

MIN_FACES = 64        # real bodies: 70k+ faces;  slivers: <= 4
MIN_VOL   = 1.0       # mm^3; real bodies: ~10^4; slivers: ~0

def main(path):
    m = trimesh.load(path)
    if isinstance(m, trimesh.Scene):                       # STL is a soup, but be defensive
        m = m.dump(concatenate=True)
    comps = m.split(only_watertight=False)
    keep = [c for c in comps if len(c.faces) >= MIN_FACES or abs(c.volume) >= MIN_VOL]
    drop = [c for c in comps if c not in keep]
    if not drop:
        print(f"strip_slivers: no slivers in {os.path.basename(path)} ({len(keep)} bodies, unchanged)")
        return
    trimesh.util.concatenate(keep).export(path)
    print(f"strip_slivers: dropped {len(drop)} sliver(s), kept {len(keep)} bodies -> {os.path.basename(path)}")
    for c in drop:
        print(f"   dropped: {len(c.faces)} faces, |vol| {abs(c.volume):.2e} mm^3")

if __name__ == "__main__":
    HERE = os.path.dirname(os.path.abspath(__file__))
    main(sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "renders/food/print_plate.stl"))
