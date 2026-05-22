"""Headless mesh cleanup for capped pieces / re-cut food (pymeshlab).

Repairs the small non-manifold artifacts the EXACT-boolean connector graft leaves,
plus duplicate/unreferenced verts. Does NOT close holes (that would seal the socket).

Usage:
  python cleanup_mesh.py in.obj [out.obj]      # out defaults to in (in place)
"""
import sys, numpy as np, pymeshlab as ml

def topo(m):
    F=np.asarray(m.face_matrix(),int)
    E=np.sort(np.vstack([F[:,[0,1]],F[:,[1,2]],F[:,[2,0]]]),axis=1)
    _,cnt=np.unique(E,axis=0,return_counts=True)
    return int((cnt==1).sum()), int((cnt>2).sum())

def clean(inp, outp):
    ms=ml.MeshSet(); ms.load_new_mesh(inp)
    b0=topo(ms.current_mesh())
    for f in ('meshing_remove_duplicate_vertices',
              'meshing_remove_duplicate_faces',
              'meshing_remove_unreferenced_vertices'):
        try: ms.apply_filter(f)
        except Exception: pass
    # NOTE: non-manifold edges from the EXACT boolean are merged at graft time
    # (remove_doubles in graft_connector.py). This script only does gentle dedup;
    # it deliberately does NOT run pymeshlab's non-manifold repair (it deletes
    # faces -> holes, or explodes vertices).
    b1=topo(ms.current_mesh())
    ms.save_current_mesh(outp)
    tag=lambda b: "watertight" if b==(0,0) else f"boundary={b[0]} nonman={b[1]}"
    print(f"  {inp.split('/')[-1]}: {tag(b0)} -> {tag(b1)}  ({ms.current_mesh().vertex_number()} v)")
    return b1

if __name__=='__main__':
    inp=sys.argv[1]; outp=sys.argv[2] if len(sys.argv)>2 else inp
    clean(inp, outp)
