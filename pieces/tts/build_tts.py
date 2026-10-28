#!/usr/bin/env python3
"""ORGANISM for Tabletop Simulator: every asset, and the save that uses them.

  pieces/tts/out/
    assets/          meshes, board art, tokens, cards, rulebook -- host these
    Organism.json    the TTS save; its URLs point at BASE
    Organism.png     the save's thumbnail

What it reuses rather than redoes:
  pieces         resources/public/organism/3d/*.glb -- make_web3d.py already
                 reduced the print STLs to a few thousand faces each
  board fit      make_web3d.fit_board finds each printed circle on the 54 cm
                 art, so snap points sit on the circles as printed
  geometry       export_tts_layout.clj -- the spaces and home spaces of every
                 board, from board.cljc and render_four_player_board.clj
  print art      ~/Downloads/organism/prototype (the 300 dpi print masters)

Boards: 2-3 players on the 40 cm board, 4 on the four-player board, 5 and 6
on the two faces of the 54 cm board (pentagon and hex). The table starts
empty but for the shared pieces; the Setup buttons lay out a board, its snap
points, and each player's pieces beside their home spaces.

    make tts BASE=https://elephantlaboratories.com/tts/organism/      (from pieces/)
    python tts/build_tts.py --save      only the save, from the assets already built
"""
import json, math, os, pathlib, random, re, subprocess, sys

import numpy as np
import trimesh
from PIL import Image, ImageDraw, ImageFont
from scipy import ndimage
from skimage.feature import peak_local_max

Image.MAX_IMAGE_PIXELS = None
HERE = pathlib.Path(__file__).resolve().parent
PIECES = HERE.parent
ROOT = PIECES.parent
sys.path.insert(0, str(PIECES))
import make_web3d  # noqa: E402

OUT = HERE / "out"
ASSETS = OUT / "assets"
WEB3D = ROOT / "resources" / "public" / "organism" / "3d"
ART = pathlib.Path(os.environ.get("BOARD_ART", pathlib.Path.home() / "Downloads/organism/prototype"))
SMALL_ART = pathlib.Path(os.environ.get(
    "SMALL_BOARD_ART", pathlib.Path.home() / "Downloads/Organism_Board_2-3_BACKSIDE_40cm_26_V1.png"))
BASE = os.environ.get("BASE", "file://" + str(ASSETS) + "/")
if not BASE.endswith("/"):
    BASE += "/"

MM = 1 / 25.4            # a TTS unit is an inch
BOARD_PX = 4096          # board textures; TTS handles 4k comfortably
META = json.loads((WEB3D / "board.json").read_text())

# Player colours in seating order. The names are the token sheets' (and
# board.json's); the colours are the real pieces'.
PLAYERS = ["Green", "Red", "Blue", "Yellow", "Purple", "Dark"]
# the TTS seat each colour sits in: TTS has no slate, so Dark sits at Teal
SEATS = {"Green": "Green", "Red": "Red", "Blue": "Blue", "Yellow": "Yellow",
         "Purple": "Purple", "Dark": "Teal"}
SEAT_RGB = {"Green": (0.192, 0.701, 0.168), "Red": (0.856, 0.1, 0.094), "Blue": (0.118, 0.53, 1.0),
            "Yellow": (0.905, 0.898, 0.172), "Purple": (0.627, 0.125, 0.941),
            "Teal": (0.129, 0.694, 0.607)}
SHEETS = {"Green_Red": ("Green", "Red"), "Purple_Blue": ("Purple", "Blue"),
          "Yellow_Dark": ("Yellow", "Dark")}
FOOD_COLOR = "#f2e6a0"   # view3d.cljs
BOARD_MM = {"main": 540.0, "small": 400.0}
FOUR_MM_PER_SPACE = 40.0  # the four-player board has no print; between hex (43) and its 37 mm study
ELEMENTS = ["EAT", "GROW", "MOVE"]
TAG = "OrganismSetup"     # global.lua's TAG: what Setup clears
# real sizes, from PRINT_GUIDE.md
POWER_MM, AID_MM, MUTATION_MM, TOKEN_MM = 165.0, 190.0, 92.0, 37.0
POWER_AT = (-20.0, 3.0, 0.0)  # where the shared things stand until Setup moves them aside
OFF_TABLE = -60.0             # x of the aid and rulebook, past the custom table's edge (+-55)
PER_PLAYER = 5            # rulebook: 5 elements of each type per player


def url(name):
    return BASE + name


def rgb(hex_):
    h = hex_.lstrip("#")
    return {"r": int(h[0:2], 16) / 255, "g": int(h[2:4], 16) / 255, "b": int(h[4:6], 16) / 255}


# ── Meshes ──────────────────────────────────────────────────────────────────

def y_up_inches(mesh, scale=1.0):
    """The web meshes are millimetres, Z up, base at z = 0; TTS wants Y up."""
    m = mesh.copy()
    m.apply_transform(trimesh.transformations.rotation_matrix(-math.pi / 2, [1, 0, 0]))
    m.apply_scale(scale * MM)
    return m


def cylinder_obj(path, radius, height, sections=20):
    c = trimesh.creation.cylinder(radius=radius, height=height, sections=sections)
    c.apply_translation([0, 0, height / 2])
    y_up_inches(c).export(path)


def pieces():
    """EAT/GROW/MOVE/FOOD as OBJ, plus a simple collider each: a cylinder of
    the piece's footprint, so stacking food and knocking pieces stays cheap."""
    sizes = {}
    for name in ELEMENTS + ["FOOD"]:
        scale = META["food_scale"] if name == "FOOD" else META["piece_scale"]
        mesh = trimesh.load(WEB3D / f"{name}.glb", force="mesh")
        y_up_inches(mesh, scale).export(ASSETS / f"{name}.obj")
        w = META["pieces"][name]["width"] * scale
        h = META["pieces"][name]["height"] * scale
        cylinder_obj(ASSETS / f"{name}_collider.obj", w * 0.42, h)
        sizes[name] = {"width": w * MM, "height": h * MM}
        print(f"  {name}: {len(mesh.faces):,} faces, {w:.0f} x {h:.0f} mm")
    cylinder_obj(ASSETS / "POWER.obj", 10, 6, 32)  # a power level marker
    return sizes


# ── Boards ──────────────────────────────────────────────────────────────────
# Each board's spaces are given in image coordinates, u right and v down, both
# -1..1 across the square image -- the Lua turns them into table positions.

def uv(px, py, w):
    return [round((px - w / 2) / (w / 2), 5), round((py - w / 2) / (w / 2), 5)]


def save_board(img, name):
    img.convert("RGB").resize((BOARD_PX, BOARD_PX), Image.LANCZOS).save(ASSETS / name, quality=92)


def printed_board(layout, file, rotation):
    """The 54 cm art, as printed; snap points on its circles where they are."""
    art = Image.open(ART / file).convert("RGBA")
    pts = [dict(p) for p in layout["spaces"]]
    s, r, target, printed = make_web3d.fit_board(pts, art, rotation)
    W = art.size[0]
    print(f"  {file}: {s * BOARD_MM['main'] / W:.1f} mm a space, rotated {r:.1f} deg")
    return art, {k: uv(x, y, W) for k, (x, y) in printed.items()}, s * BOARD_MM["main"] / W


def small_board(layout):
    """The 40 cm 2-3 player board is line art on black, so fit the game's
    lattice to the brightness of the circle outlines, then each circle."""
    art = Image.open(SMALL_ART).convert("RGB")
    W = art.size[0]
    grey = np.asarray(art.convert("L"), dtype=float)
    rel = [(f"{p['ring']}-{p['n']}", p["x"], p["y"]) for p in layout["spaces"]]

    def rim(x, y, rad):
        q = np.linspace(0, 2 * math.pi, 48, endpoint=False)
        xs = np.clip((x + rad * np.cos(q)).astype(int), 0, W - 1)
        ys = np.clip((y + rad * np.sin(q)).astype(int), 0, W - 1)
        return grey[ys, xs].mean()

    def place(s, r):
        t = math.radians(r)
        return [(k, W / 2 + s * (x * math.cos(t) - y * math.sin(t)),
                 W / 2 + s * (x * math.sin(t) + y * math.cos(t))) for k, x, y in rel]

    _, s, r, f = max((np.mean([rim(x, y, s * f) for _, x, y in place(s, r)]), s, r, f)
                     for s in np.arange(200, 300, 1.0) for r in (0.0, 30.0, -30.0)
                     for f in np.arange(0.40, 0.50, 0.01))
    printed = {}
    for k, x, y in place(s, r):
        _, bx, by = max((rim(x + dx, y + dy, s * f), x + dx, y + dy)
                        for dx in range(-8, 9) for dy in range(-8, 9))
        printed[k] = uv(bx, by, W)
    mm = s * BOARD_MM["small"] / W
    print(f"  small: {mm:.1f} mm a space, rotated {r:.0f} deg, circles r = {f:.2f} spaces")
    return art, printed, mm


def four_board(layout):
    """The four-player board has no print: render its SVG (from the layout
    export) and take the space centres straight from the layout."""
    svg = OUT / "four-player-board.svg"
    png = OUT / "four-player-board.png"
    subprocess.run(["rsvg-convert", "-w", str(BOARD_PX), "-h", str(BOARD_PX), "-o", str(png), str(svg)],
                   check=True)
    art = layout["art"]
    px, (cx, cy), unit = art["px"], art["centre"], art["unit"]
    spaces = {f"{p['ring']}-{p['n']}": uv(cx + p["x"] * unit, cy + p["y"] * unit, px)
              for p in layout["spaces"]}
    diameter = px / unit * FOUR_MM_PER_SPACE
    print(f"  four: {FOUR_MM_PER_SPACE:.0f} mm a space, {diameter:.0f} mm across")
    return Image.open(png), spaces, diameter


def boards(layout):
    out = {}
    hex_art, hex_spaces, hex_mm = printed_board(layout["hex"], "27_HEX_54cm_01.png", 30.0)
    penta_art, penta_spaces, penta_mm = printed_board(layout["penta"], "27_Pent_54cm_01.png", 24.0)
    save_board(hex_art, "board-hex.jpg")
    save_board(penta_art, "board-penta.jpg")
    small_art, small_spaces, small_mm = small_board(layout["small"])
    save_board(small_art, "board-small.jpg")
    four_art, four_spaces, four_mm = four_board(layout["four"])
    save_board(four_art, "board-four.jpg")
    out["hex"] = {"object": "main", "face": "front", "spaces": hex_spaces, "space_mm": hex_mm,
                  "homes": layout["hex"]["homes"]}
    out["penta"] = {"object": "main", "face": "back", "spaces": penta_spaces, "space_mm": penta_mm,
                    "homes": layout["penta"]["homes"]}
    out["small"] = {"object": "small", "face": "front", "spaces": small_spaces, "space_mm": small_mm,
                    "homes": layout["small"]["homes"]}
    out["four"] = {"object": "four", "face": "front", "spaces": four_spaces, "space_mm": FOUR_MM_PER_SPACE,
                   "homes": layout["four"]["homes"]}
    return out, {"main": BOARD_MM["main"], "small": BOARD_MM["small"], "four": four_mm}


# ── Print sheets: tokens and mutation cards ─────────────────────────────────

def find_circles(path, radius, count, scale=2):
    """Circle centres on a print sheet. The tokens touch, so neither a flood
    fill nor edge detection separates them reliably; but each is solid colour
    on white, so its centre is the point farthest from the white."""
    a = np.asarray(Image.open(path).convert("RGB"))[::scale, ::scale]
    dist = ndimage.distance_transform_edt(a.min(axis=2) < 235)
    peaks = peak_local_max(dist, min_distance=int(radius / scale * 0.8),
                           threshold_abs=radius / scale * 0.85, num_peaks=count)
    return [(x * scale, y * scale, dist[y, x] * scale) for y, x in peaks]


def crop_circle(img, x, y, r, size):
    return img.crop((x - r, y - r, x + r, y + r)).resize((size, size), Image.LANCZOS)


def reading_order(circles):
    """Row by row, left to right: a circle starts a new row once it sits more
    than a radius below the row's first."""
    rows = []
    for c in sorted(circles, key=lambda c: c[1]):
        if rows and c[1] - rows[-1][0][1] < c[2]:
            rows[-1].append(c)
        else:
            rows.append([c])
    return [c for row in rows for c in sorted(row, key=lambda c: c[0])]


def tokens():
    """Per colour: nine numbered tokens (the dots count 1-9) and nine element
    discs, three each of EAT (star), GROW (clover), MOVE (spiral)."""
    out = {}
    for sheet, (top, bottom) in SHEETS.items():
        path = ART / f"{sheet}-01.png"
        found = find_circles(path, 218, 36)
        rgbimg = Image.open(path).convert("RGB")
        if len(found) != 36:
            raise SystemExit(f"{sheet}: found {len(found)} tokens, expected 36")
        # four blocks down the sheet: numbers (top colour, bottom colour), then
        # discs (bottom colour, top colour) -- read by row, then column
        rows = sorted(found, key=lambda c: c[1])
        blocks = [reading_order(rows[i:i + 9]) for i in range(0, 36, 9)]
        for color, block, kind in [(top, blocks[0], "number"), (bottom, blocks[1], "number"),
                                   (bottom, blocks[2], "disc"), (top, blocks[3], "disc")]:
            for i, (x, y, r) in enumerate(block):
                name = f"token-{color}-{kind}-{i + 1}.jpg"
                crop_circle(rgbimg, x, y, r, 512).save(ASSETS / name, quality=90)
                out.setdefault(color, {}).setdefault(kind, []).append(name)
    # disc order on the sheet, per row of a block: MOVE EAT GROW MOVE / EAT GROW MOVE EAT GROW
    disc_kinds = ["MOVE", "EAT", "GROW", "MOVE", "EAT", "GROW", "MOVE", "EAT", "GROW"]
    for color in out:
        out[color]["disc_kinds"] = disc_kinds
    print(f"  tokens: {sum(len(v['number']) + len(v['disc']) for v in out.values())}")
    return out


def mutation_cards():
    names = []
    for path in sorted((ART / "mutations").glob("MutationCard*.png"),
                       key=lambda p: int(re.sub(r"\D", "", p.stem) or 1)):
        found = find_circles(path, 699, 2)
        rgbimg = Image.open(path).convert("RGB")
        for x, y, r in sorted(found):
            name = f"mutation-{len(names) + 1:02d}.jpg"
            crop_circle(rgbimg, x, y, r, 1024).save(ASSETS / name, quality=90)
            names.append(name)
    # no back was ever drawn; a plain one in the card's own colours
    back = Image.new("RGB", (1024, 1024), "#161c22")
    d = ImageDraw.Draw(back)
    d.ellipse([40, 40, 984, 984], outline="#3d5662", width=10)
    font = ImageFont.truetype(str(PIECES / "inputs" / "JTEnergyVF.ttf"), 120)
    d.text((512, 512), "MUTATION", fill="#c9d6dc", font=font, anchor="mm")
    back.save(ASSETS / "mutation-back.jpg", quality=90)
    print(f"  mutation cards: {len(names)}")
    return names


def sheets():
    for src, name, size in [("ScoreCard_01_print.png", "power-board.jpg", 2048),
                            ("25_PLAYER-AID_09_print.png", "player-aid.jpg", 2048)]:
        Image.open(ART / src).convert("RGB").resize((size, size), Image.LANCZOS).save(ASSETS / name, quality=92)
    # the custom table: plain, in the boards' own slate, darker at the rim
    w, h = 2200, 1500
    yy, xx = np.mgrid[0:h, 0:w]
    r = np.hypot((xx - w / 2) / (w / 2), (yy - h / 2) / (h / 2))
    base = np.array([0x24, 0x37, 0x46], dtype=float)
    shade = (1.0 - 0.35 * np.clip(r - 0.3, 0, 1))[..., None]
    grain = np.random.default_rng(7).normal(0, 2.0, (h, w, 1))
    Image.fromarray(np.clip(base * shade + grain, 0, 255).astype(np.uint8)).save(ASSETS / "table.jpg", quality=90)
    pdf = PIECES / "prototype/print_ready/04_RULEBOOK/rulebook_18pp.pdf"
    (ASSETS / "rulebook.pdf").write_bytes(pdf.read_bytes())


# ── The save ────────────────────────────────────────────────────────────────

_guids = set()


def guid():
    while True:
        g = "%06x" % random.getrandbits(24)
        if g not in _guids:
            _guids.add(g)
            return g


def obj(name, nick="", pos=(0, 1, 0), rot=(0, 0, 0), scale=(1, 1, 1), color="#ffffff",
        locked=False, desc="", tags=(), **extra):
    o = {"GUID": guid(), "Name": name,
         "Transform": {"posX": pos[0], "posY": pos[1], "posZ": pos[2],
                       "rotX": rot[0], "rotY": rot[1], "rotZ": rot[2],
                       "scaleX": scale[0], "scaleY": scale[1], "scaleZ": scale[2]},
         "Nickname": nick, "Description": desc, "GMNotes": "", "ColorDiffuse": rgb(color),
         "Locked": locked, "Grid": True, "Snap": True, "IgnoreFoW": False, "Autoraise": True,
         "Sticky": True, "Tooltip": True, "GridProjection": False, "Hands": False,
         "LuaScript": "", "LuaScriptState": "", "XmlUI": ""}
    if tags:
        o["Tags"] = list(tags)
    o.update(extra)
    return o


def model(mesh, nick, color, collider=None, material=0, type_index=0, **kw):
    return obj("Custom_Model", nick, color=color, CustomMesh={
        "MeshURL": url(mesh), "DiffuseURL": "", "NormalURL": "",
        "ColliderURL": url(collider) if collider else "", "Convex": True,
        "MaterialIndex": material, "TypeIndex": type_index,
        "CustomShader": {"SpecularColor": {"r": 1, "g": 1, "b": 1}, "SpecularIntensity": 0.15,
                         "SpecularSharpness": 3.0, "FresnelStrength": 0.1},
        "CastShadows": True}, **kw)


def tile(image, nick, mm, back=None, shape=2, thickness=0.1, stackable=False, **kw):
    """shape: 0 box, 1 hex, 2 circle, 3 rounded box. `mm` across: the Lua
    scales the tile to it, as TTS sizes a custom tile by its own rule."""
    return obj("Custom_Tile", nick, GMNotes=f"inches={mm * MM:.3f}", CustomImage={
        "ImageURL": url(image), "ImageSecondaryURL": url(back) if back else "",
        "ImageScalar": 1.0, "WidthScale": 0.0,
        "CustomTile": {"Type": shape, "Thickness": thickness, "Stackable": stackable, "Stretch": True}},
        **kw)


def bag(nick, contents, color="#555555", infinite=False, **kw):
    return obj("Infinite_Bag" if infinite else "Bag", nick, color=color,
               ContainedObjects=contents, **kw)


def piece(kind, color_name):
    return model(f"{kind}.obj", f"{color_name} {kind}", META["piece_colors"][color_name],
                 collider=f"{kind}_collider.obj", type_index=1)


def food():
    return model("FOOD.obj", "Food", FOOD_COLOR, collider="FOOD_collider.obj")


def templates(tok, board_mm):
    """What Setup spawns, as TTS object data."""
    t = {"boards": {
        "main": tile("board-hex.jpg", "Board (hex: 6 players / flip for pentagon: 5)", board_mm["main"],
                     back="board-penta.jpg", thickness=0.2),
        "small": tile("board-small.jpg", "Board (2-3 players)", board_mm["small"], shape=3, thickness=0.2),
        "four": tile("board-four.jpg", "Board (4 players)", board_mm["four"], thickness=0.2)},
        "board_inches": {k: v * MM for k, v in board_mm.items()},
        "food": food(), "power_at": POWER_AT, "players": {}}
    for color in PLAYERS:
        hexc = META["piece_colors"][color]
        numbers = [tile(n, f"{color} {i + 1}", TOKEN_MM, thickness=0.08, stackable=True)
                   for i, n in enumerate(tok[color]["number"])]
        discs = [tile(n, f"{color} {k} disc", TOKEN_MM, thickness=0.08, stackable=True)
                 for n, k in zip(tok[color]["disc"], tok[color]["disc_kinds"])]
        t["players"][color] = {
            "color": rgb(hexc),
            "piece": {k: piece(k, color) for k in ELEMENTS},
            "bag": {k: bag(f"{color} {k}", [piece(k, color) for _ in range(PER_PLAYER - 1)], hexc)
                    for k in ELEMENTS},
            "power": model("POWER.obj", f"{color} power level", hexc, type_index=5),
            "tokens": bag(f"{color} tokens", numbers + discs, hexc)}
    tag(t)
    return t


def tag(tree):
    """Everything Setup spawns carries the tag it clears by, bag contents too."""
    if isinstance(tree, dict):
        if "GUID" in tree:
            tree["Tags"] = [TAG]
        for v in tree.values():
            tag(v)
    elif isinstance(tree, list):
        for v in tree:
            tag(v)


def build():
    ASSETS.mkdir(parents=True, exist_ok=True)
    layout_path = OUT / "layout.json"
    if not layout_path.exists():
        subprocess.run(["lein", "run", "-m", "clojure.main", str(HERE / "export_tts_layout.clj"), str(OUT)],
                       cwd=ROOT, check=True)
    layout = json.loads(layout_path.read_text())
    cache = OUT / "assets.json"
    if "--save" in sys.argv and cache.exists():
        # the assets are built; only rewrite the save (the Lua, the layout of the table)
        sizes, board_cfg, board_mm, tok, cards = json.loads(cache.read_text())
    else:
        print("pieces")
        sizes = pieces()
        print("boards")
        board_cfg, board_mm = boards(layout)
        print("print sheets")
        tok = tokens()
        cards = mutation_cards()
        sheets()
        cache.write_text(json.dumps([sizes, board_cfg, board_mm, tok, cards]))

    config = {"boards": board_cfg, "players": PLAYERS, "seats": SEATS, "sizes": sizes,
              "plans": {"2": "small", "3": "small", "4": "four", "5": "penta", "6": "hex"},
              "templates": templates(tok, board_mm)}
    lua = (HERE / "global.lua").read_text().replace(
        "--CONFIG--", "CONFIG_JSON = [==[" + json.dumps(config, separators=(",", ":")) + "]==]")
    xml = (HERE / "global.xml").read_text()

    shared = [
        tile("power-board.jpg", "Power board", POWER_MM, pos=POWER_AT, locked=True),
        bag("Mutations", [tile(c, f"Mutation {i + 1}", MUTATION_MM, back="mutation-back.jpg",
                               thickness=0.05, stackable=True)
                          for i, c in enumerate(cards)], "#2a3a44", pos=(POWER_AT[0], 1.5, 6)),
        bag("Food", [food()], FOOD_COLOR, infinite=True, pos=(POWER_AT[0], 1.5, -6)),
        # the aid and the rulebook stand off the table's edge, to hover over
        # (alt) or open, not to take room on it
        tile("player-aid.jpg", "Player aid", AID_MM, pos=(OFF_TABLE, 2.0, 5.0), rot=(0, 90, 0),
             thickness=0.05, locked=True),
        obj("Custom_PDF", "Rulebook", pos=(OFF_TABLE, 2.0, -5.0), rot=(0, 90, 0), locked=True,
            CustomPDF={"PDFUrl": url("rulebook.pdf"), "PDFPassword": "", "PDFPage": 0, "PDFPageOffset": 0}),
    ]
    # a seat (hand zone) per colour, round the table as for six; Setup moves
    # each player's seat behind their own pieces
    for i, color in enumerate(PLAYERS):
        seat = SEATS[color]
        a = math.radians(60 * i)
        x, z = 24 * math.sin(a), -24 * math.cos(a)
        hand = obj("HandTrigger", pos=(x, 3.5, z), rot=(0, math.degrees(math.atan2(-x, -z)) % 360, 0),
                   scale=(11.7, 9.2, 6.8), locked=True, FogColor=seat)
        hand["ColorDiffuse"] = dict(zip("rgb", SEAT_RGB[seat]), a=0.0)
        shared.append(hand)
    save = {"SaveName": "ORGANISM", "GameMode": "ORGANISM", "GameType": "Game",
            "GameComplexity": "Medium", "Tags": ["Strategy", "Abstract"], "Gravity": 0.5,
            "PlayArea": 0.8, "Table": "Table_Custom", "TableURL": url("table.jpg"), "Sky": "Sky_Museum", "Note": "",
            "Rules": "", "XmlUI": xml, "LuaScript": lua, "LuaScriptState": "",
            "Grid": {"Type": 0, "Lines": False, "Snapping": False, "Offset": False, "BothSnapping": False,
                     "xSize": 2.0, "ySize": 2.0, "PosOffset": {"x": 0, "y": 1, "z": 0}},
            "Hands": {"Enable": True, "DisableUnused": False, "Hiding": 0},
            "ObjectStates": shared}
    (OUT / "Organism.json").write_text(json.dumps(save, indent=1))
    box = Image.open(PIECES / "clip_assets" / "box_top.png").convert("RGB")
    box.resize((256, 256), Image.LANCZOS).save(OUT / "Organism.png")
    print(f"wrote {OUT / 'Organism.json'} (assets at {BASE})")


if __name__ == "__main__":
    build()
