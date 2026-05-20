"""
Stage 6 — remesh: isotropic explicit remeshing of the closed surface.

Decouples mesh quality from surface shape: given any valid closed manifold, make
all triangles ~target_edge and near-equilateral, regardless of how steep the
height field was. Sharp features (the footprint rim) are preserved by angle.
"""
from __future__ import annotations
import numpy as np
import pymeshlab as ml


def isotropic_remesh(V, F, target_edge=1.0, iterations=10, feature_deg=40.0):
    ms = ml.MeshSet()
    ms.add_mesh(ml.Mesh(vertex_matrix=np.asarray(V, float),
                        face_matrix=np.asarray(F, np.int32)))
    ms.meshing_isotropic_explicit_remeshing(
        targetlen=ml.PureValue(target_edge),
        iterations=iterations,
        adaptive=False,                 # uniform sizing
        featuredeg=feature_deg,         # keep the rim crisp, smooth the body
    )
    m = ms.current_mesh()
    return m.vertex_matrix(), m.face_matrix()
