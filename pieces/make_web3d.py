#!/usr/bin/env python3
"""Assets for the game page's 3D view: the pieces, and the printed boards.

  resources/public/organism/3d/
    EAT.glb GROW.glb MOVE.glb FOOD.glb   the print pieces, reduced for the web
    board-hex.webp board-penta.webp      the printed boards, lined up with the game
    board.json                           how a game space maps onto them

Pieces. The canonical print meshes (pieces/stl/, millimetres, Z up, base at
z=0) are 70-180 thousand faces and 4-12 MB -- far too heavy for a page. Each
is reduced by quadric edge collapse, which keeps the silhouette and the
sculpted relief, to a budget a browser draws without effort, and written as
binary glTF with normals. The player colour comes from the material at run
time, so one mesh per piece type serves every player.

Boards. The printed art (27_HEX / 27_Pent, 54 cm, 6324 px) was laid out by
hand, not generated from the game's geometry, so it does not line up with the
game's spaces by any single transform -- the hex board nearly does, the penta
board drifts in its outer rings. So: fit the game's layout to the art
globally (scale, rotation, centre), then find each printed circle's own
centre, and redraw each circle exactly where the game puts that space. The
result is a texture that is the game's geometry under one similarity
transform, which the page maps onto a single plane.

The game layout comes from board.cljc itself (export_board_layout.clj), so
the texture follows the rules' geometry rather than a second copy of it.

    make web3d            (from pieces/)
"""
import json, math, os, pathlib, subprocess, sys

import numpy as np
import pymeshlab
import trimesh
from PIL import Image, ImageDraw

Image.MAX_IMAGE_PIXELS = None
HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent
OUT = ROOT / "resources" / "public" / "organism" / "3d"
ART = pathlib.Path(os.environ.get("BOARD_ART", pathlib.Path.home() / "Downloads/organism/prototype"))
BOARDS = {"hex": ("6", "27_HEX_54cm_01.png", 30.0), "penta": ("5", "27_Pent_54cm_01.png", 24.0)}
BOARD_MM = 540.0
TEXTURE_PX = 2048
FACES = {"EAT": 7000, "GROW": 8000, "MOVE": 9000, "FOOD_slip": 2500}
# The colours of the real pieces: five sampled from the printed colour sheets
# (the same values as build_setup_scene.py), and red from the rulebook, which
# names six. Six players is also as far as the printed board goes.
PIECE_COLORS = {"Purple": "#9C6E90", "Blue": "#6DA2B2", "Green": "#4BA166",
                "Yellow": "#DCAD3F", "Dark": "#4F6573", "Red": "#BA6A75"}


# ── Pieces ──────────────────────────────────────────────────────────────────

def reduce_piece(name, budget):
    ms = pymeshlab.MeshSet()
    ms.load_new_mesh(str(HERE / "stl" / f"{name}.stl"))
    before = ms.current_mesh().face_number()
    ms.meshing_decimation_quadric_edge_collapse(
        targetfacenum=budget, preservenormal=True, preservetopology=True,
        planarquadric=True, qualitythr=0.5, optimalplacement=True)
    m = ms.current_mesh()
    mesh = trimesh.Trimesh(vertices=m.vertex_matrix(), faces=m.face_matrix(), process=True)
    # centred on its footprint, standing on z = 0, as it stands on the board
    lo, hi = mesh.bounds
    mesh.apply_translation([-(lo[0] + hi[0]) / 2, -(lo[1] + hi[1]) / 2, -lo[2]])
    out = OUT / (("FOOD" if name.startswith("FOOD") else name) + ".glb")
    mesh.export(out)
    print(f"  {name}: {before:,} -> {len(mesh.faces):,} faces, "
          f"{out.stat().st_size / 1024:.0f} KB, height {mesh.extents[2]:.1f} mm")
    return {"height": float(mesh.extents[2]), "width": float(max(mesh.extents[:2]))}


# ── Boards ──────────────────────────────────────────────────────────────────

def game_layout():
    """Every space of a full 7-ring board, per symmetry, from board.cljc."""
    cached = OUT / ".layout.json"
    subprocess.run(["lein", "run", "-m", "clojure.main", str(HERE / "export_board_layout.clj"),
                    str(cached)], cwd=ROOT, check=True, capture_output=True)
    return json.loads(cached.read_text())


def fit_board(layout, art, rotation):
    """Scale and rotation that best put the game's spaces on the printed circles,
    then each circle's own centre. Brightness is the evidence: a printed circle
    is lighter than the dark rim around it."""
    grey = np.asarray(art.convert("L").resize((art.size[0] // 4,) * 2), dtype=float)
    W, H = art.size[0], grey.shape[0]
    centre = [p for p in layout if p["ring"] == "A"][0]
    rel = [(f"{p['ring']}-{p['n']}", p["x"] - centre["x"], p["y"] - centre["y"]) for p in layout]

    def px(x, y):
        return grey[min(max(int(y), 0), H - 1), min(max(int(x), 0), H - 1)]

    def place(s, r):
        t = math.radians(r)
        return [(k, W / 2 + s * (x * math.cos(t) - y * math.sin(t)),
                 W / 2 + s * (x * math.sin(t) + y * math.cos(t))) for k, x, y in rel]

    def score(s, r):
        total = 0.0
        for _, x, y in place(s, r):
            X, Y = x / 4, y / 4
            inner = np.mean([px(X + i, Y + j) for i in (-2, 0, 2) for j in (-2, 0, 2)])
            gap = np.mean([px(X + s / 8 * math.cos(q), Y + s / 8 * math.sin(q))
                           for q in np.linspace(0, 2 * math.pi, 12, endpoint=False)])
            total += inner - gap
        return total / len(rel)

    _, s, r = max((score(s, r), s, r) for s in np.linspace(400, 560, 81)
                  for r in np.arange(rotation - 3, rotation + 3.01, 0.5))
    _, s = max((score(ss, r), ss) for ss in np.linspace(s - 4, s + 4, 17))

    grey2 = np.asarray(art.convert("L").resize((W // 2,) * 2), dtype=float)
    H2 = grey2.shape[0]

    def px2(x, y):
        return grey2[min(max(int(y), 0), H2 - 1), min(max(int(x), 0), H2 - 1)]

    def circleness(x, y, rad):
        rim = [px2(x + rad * math.cos(q), y + rad * math.sin(q)) for q in np.linspace(0, 2 * math.pi, 24, endpoint=False)]
        inner = [px2(x + 0.6 * rad * math.cos(q), y + 0.6 * rad * math.sin(q)) for q in np.linspace(0, 2 * math.pi, 12, endpoint=False)]
        return np.mean(inner) - np.mean(rim) - 0.5 * np.std(rim)

    printed = {}
    reach = int(s * 0.25 / 2)
    for k, x, y in place(s, r):
        X, Y, rad = x / 2, y / 2, s * 0.5 / 2 * 0.95
        best = max((circleness(X + dx, Y + dy, rad), X + dx, Y + dy)
                   for dx in range(-reach, reach + 1, 3) for dy in range(-reach, reach + 1, 3))
        best = max((circleness(best[1] + dx, best[2] + dy, rad), best[1] + dx, best[2] + dy)
                   for dx in range(-3, 4) for dy in range(-3, 4))
        printed[k] = (best[1] * 2, best[2] * 2)
    return s, r, place(s, r), printed


def warp_board(art, target, printed, s):
    """Redraw each printed circle where the game puts its space."""
    out = art.copy()
    size = int(s * 1.02)
    mask = Image.new("L", (size, size), 0)
    ImageDraw.Draw(mask).ellipse([0, 0, size - 1, size - 1], fill=255)
    for k, x, y in target:
        px_, py_ = printed[k]
        patch = art.crop((int(px_ - size / 2), int(py_ - size / 2),
                          int(px_ - size / 2) + size, int(py_ - size / 2) + size))
        out.paste(patch, (int(x - size / 2), int(y - size / 2)), mask)
    return out


def build_board(name, layout_all):
    sym, file, rotation = BOARDS[name]
    art = Image.open(ART / file).convert("RGBA")
    W = art.size[0]
    s, r, target, printed = fit_board(layout_all[sym], art, rotation)
    moved = [math.hypot(printed[k][0] - x, printed[k][1] - y) / s for k, x, y in target]
    warped = warp_board(art, target, printed, s)
    out = OUT / f"board-{name}.webp"
    warped.resize((TEXTURE_PX, TEXTURE_PX), Image.LANCZOS).save(out, "WEBP", quality=86, method=6)
    # the colour of the board between and beyond the rings, for covering printed
    # circles a smaller game does not have
    bg = warped.convert("RGB").getpixel((int(W * 0.5), int(W * 0.03)))
    print(f"  {name}: {s * BOARD_MM / W:.2f} mm a space, rotated {r:.1f} deg; circles moved "
          f"median {np.median(moved):.3f}, max {max(moved):.3f} spaces; "
          f"{out.stat().st_size / 1024:.0f} KB")
    return {"symmetry": int(sym), "rotation": r, "mm_per_space": s * BOARD_MM / W,
            "board_mm": BOARD_MM, "rings": 7, "texture": out.name,
            "base": "#%02x%02x%02x" % bg[:3]}


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    print("pieces")
    pieces = {name if not name.startswith("FOOD") else "FOOD": reduce_piece(name, budget)
              for name, budget in FACES.items()}
    print("boards")
    layout = game_layout()
    boards = {name: build_board(name, layout) for name in BOARDS}
    meta = {"pieces": pieces, "boards": boards, "piece_colors": PIECE_COLORS,
            # how the Blender scenes seat things, so the page matches the renders:
            # pieces at 0.9 on the board, food on the peg 4.3 mm below a piece's
            # top, 6.4 mm per food up the stack, at 0.94, three at most shown
            "piece_scale": 0.9, "food_scale": 0.94, "peg_below_top": 4.3,
            "food_step": 6.4, "food_shown": 3}
    (OUT / "board.json").write_text(json.dumps(meta, indent=1))
    print(f"wrote {OUT}")


if __name__ == "__main__":
    main()
