#!/usr/bin/env python3
"""Gather every game's printed and modelled components into one library for
the playtest table (src/cljs/table/, served at /table/).

Nothing here is drawn anew. Each pipeline already makes its own art -- the
UNIVERSE and DISTINCTIONS MPC faces, the TTS mod's organism tokens, mutations
and boards, the organism GLBs -- and this only shrinks those to web size and
writes library.json, which says what each thing is and how big it is in
millimetres. Re-run it (`make` in table/) after any of those pipelines changes.

    resources/public/table/library.json
    resources/public/table/assets/<game>/*.webp

A missing source is skipped with a note rather than failing the whole library,
so a checkout without (say) the TTS output still gets the other games.
"""
import csv
import json
import pathlib
import sys

from PIL import Image

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent
PUBLIC = ROOT / "resources" / "public"
OUT = PUBLIC / "table"
ASSETS = OUT / "assets"
WEB = "/table/assets/"
ORG3D = PUBLIC / "organism" / "3d"
TTS = ROOT / "pieces" / "tts" / "out"

CARD_MM = [63.5, 88.9]   # poker: UNIVERSE and DISTINCTIONS are both cut at 750 x 1050 @ 300 dpi
MPC_BLEED = 36           # px each side on the 822 x 1122 MPC faces
CARD_PX = 400            # web width of a card face
QUALITY = 86

notes = []


def webp(src, dest, width=None, crop=None, square=None):
    """Write src as WebP under ASSETS, return its URL. Skip if up to date."""
    dest = ASSETS / dest
    if dest.exists() and dest.stat().st_mtime >= src.stat().st_mtime:
        return WEB + str(dest.relative_to(ASSETS))
    dest.parent.mkdir(parents=True, exist_ok=True)
    im = Image.open(src).convert("RGB")
    if crop:
        im = im.crop((crop, crop, im.width - crop, im.height - crop))
    if square:
        im = im.resize((square, square), Image.LANCZOS)
    elif width and im.width > width:
        im = im.resize((width, round(im.height * width / im.width)), Image.LANCZOS)
    im.save(dest, "WEBP", quality=QUALITY, method=5)
    return WEB + str(dest.relative_to(ASSETS))


def mpc_deck(game, folder, label, name_of, backs):
    """A whole MPC deck: faces in manifest order, plus each back on offer."""
    manifest = folder / "manifest.csv"
    if not manifest.exists():
        notes.append(f"{game}: no {manifest}, run its `make faces` first")
        return []
    rows = list(csv.DictReader(manifest.open()))
    cards = []
    for row in rows:
        src = folder / "faces" / row["file"]
        cards.append({"name": name_of(row),
                      "face": webp(src, f"{game}/{pathlib.Path(row['file']).stem}.webp",
                                   width=CARD_PX, crop=MPC_BLEED)})
    items = []
    for back_file, back_label in backs:
        src = folder / back_file
        if not src.exists():
            continue
        back = webp(src, f"{game}/{src.stem}.webp", width=CARD_PX, crop=MPC_BLEED)
        items.append({"id": f"{game}-deck-{src.stem}", "label": f"{label} deck" +
                      (f" ({back_label})" if back_label else ""),
                      "kind": "cards", "size": CARD_MM,
                      "cards": [dict(c, back=back) for c in cards]})
    return items


def universe():
    return mpc_deck("universe", ROOT / "universe" / "out" / "mpc", "UNIVERSE",
                    lambda r: f"{r['color']} {r['shape']} {r['number']}",
                    [("back.png", None)])


def distinctions():
    def name(r):
        bits = [r["bg"], r["fg"], r["comp"]]
        bits += [b for b in ("eye", "rays", "inv") if r[b] == "True"]
        return " ".join(bits)
    return mpc_deck("distinctions", ROOT / "distinctions" / "out" / "mpc", "DISTINCTIONS", name,
                    [("back.png", None), ("back-pairs.png", "pairs back"),
                     ("back-jigsaw-3.png", "jigsaw back")])


def organism():
    meta = json.loads((ORG3D / "board.json").read_text())
    colors = meta["piece_colors"]
    items = []
    # pieces: the web GLBs are millimetres, Z up, base at z = 0
    for name in ["EAT", "GROW", "MOVE", "FOOD"]:
        scale = meta["food_scale"] if name == "FOOD" else meta["piece_scale"]
        if name == "FOOD":
            items.append({"id": "organism-FOOD", "label": "FOOD", "kind": "model",
                          "model": f"/organism/3d/{name}.glb", "zUp": True, "scale": scale,
                          "color": "#f2e6a0", "group": "Organism pieces"})
            continue
        for player, hexc in colors.items():
            items.append({"id": f"organism-{name}-{player}", "label": f"{name} ({player})",
                          "kind": "model", "model": f"/organism/3d/{name}.glb", "zUp": True,
                          "scale": scale, "color": hexc, "group": "Organism pieces"})
    for player, hexc in colors.items():
        items.append({"id": f"organism-power-{player}", "label": f"Power marker ({player})",
                      "kind": "shape", "shape": "cylinder", "size": [20, 6, 20], "color": hexc,
                      "group": "Organism pieces"})

    # the main board is one 54 cm disc: hex on the front, penta on the back
    items.append({"id": "organism-board-main", "label": "Main board (hex / penta)", "kind": "tile",
                  "shape": "round", "size": [540, 3, 540], "face": "/organism/3d/board-hex.webp",
                  "back": "/organism/3d/board-penta.webp", "edge": meta["boards"]["hex"]["base"],
                  "group": "Organism boards"})

    a = TTS / "assets"
    if not a.exists():
        notes.append(f"organism: no {a}, run `make tts` in pieces/ for tokens, mutations and the other boards")
        return items
    four = json.loads((TTS / "layout.json").read_text())["four"]["art"]
    four_mm = round(four["px"] / four["unit"] * 40.0, 1)
    for file, label, shape, mm, edge in [("board-small.jpg", "2-3 player board (40 cm)", "round", 400, "#111111"),
                                         ("board-four.jpg", "Four-player board", "rect", four_mm, None),
                                         ("power-board.jpg", "Power board", "rect", 165, None),
                                         ("player-aid.jpg", "Player aid", "rect", 190, None)]:
        items.append({"id": f"organism-{pathlib.Path(file).stem}", "label": label, "kind": "tile",
                      "shape": shape, "size": [mm, 2 if mm > 300 else 1, mm], "edge": edge,
                      "face": webp(a / file, f"organism/{pathlib.Path(file).stem}.webp", width=2048),
                      "group": "Organism boards"})

    back = webp(a / "mutation-back.jpg", "organism/mutation-back.webp", square=512)
    muts = sorted(a.glob("mutation-[0-9]*.jpg"))
    items.append({"id": "organism-mutations", "label": f"Mutations ({len(muts)})", "kind": "cards",
                  "shape": "round", "size": [92, 92], "group": "Organism cards",
                  "cards": [{"name": f"Mutation {i + 1}",
                             "face": webp(p, f"organism/{p.stem}.webp", square=512), "back": back}
                            for i, p in enumerate(muts)]})

    for player, hexc in colors.items():
        for kind, label in [("number", "number tokens"), ("disc", "element discs")]:
            files = sorted(a.glob(f"token-{player}-{kind}-*.jpg"))
            if not files:
                continue
            items.append({"id": f"organism-tokens-{player}-{kind}", "label": f"{player} {label}",
                          "kind": "set", "group": "Organism tokens",
                          "items": [{"kind": "tile", "shape": "round", "size": [37, 2, 37],
                                     "label": f.stem.replace("token-", ""),
                                     "face": webp(f, f"organism/{f.stem}.webp", square=256),
                                     "edge": hexc} for f in files]})
    return items


def main():
    games = [("Organism", organism), ("UNIVERSE", universe), ("DISTINCTIONS", distinctions)]
    library = []
    for title, make in games:
        try:
            items = make()
        except FileNotFoundError as e:
            notes.append(f"{title}: {e}")
            items = []
        if items:
            library.append({"game": title, "items": items})
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "library.json").write_text(json.dumps(library, indent=1) + "\n")
    n = sum(len(g["items"]) for g in library)
    size = sum(p.stat().st_size for p in ASSETS.rglob("*") if p.is_file()) if ASSETS.exists() else 0
    print(f"library.json: {n} components across {len(library)} games, assets {size / 1e6:.1f} MB")
    for note in notes:
        print("  skipped:", note, file=sys.stderr)


if __name__ == "__main__":
    main()
