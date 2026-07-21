"""Drop degenerate connected components ("slivers") from a plate STL, in place.

The MOVE sculpt carries ~9 near-coincident triangle specks (2-4 faces each, ~0 area/volume)
left over from sculpting. trimesh isolates each as its own tiny body, while the real pieces
(and food) are 70k+ faces and 10^4 mm^3 -- an astronomical gap -- so the (faces < MIN_FACES
AND |vol| < MIN_VOL) test can only ever remove slivers, never a real solid. Kept components are
re-concatenated unchanged (verts/faces byte-for-byte), so piece geometry is untouched.

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
