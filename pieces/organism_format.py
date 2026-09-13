"""Reader for the Organism Game Format (OGF).

OGF is a universal JSON encoding of an organism game sequence (board topology +
per-turn board snapshots). See organism.format (Clojure) for the writer/spec.

    from organism_format import load_ogf, layout, color_of
    g = load_ogf("happy-tiger.ogf.json")
    g["symmetry"]                  # board symmetry (hex=6, pentagon=5)
    pos = layout(g)                # {space: (x, y)} for placing pieces
    for frame in g["frames"]:
        for player, etype, space, food in frame["elements"]:
            x, y = pos[space]      # color_of(g, player) -> piece color
"""
import json
import math
import re
import random


STANDARD_RING_COLORS = ['#fff88c', '#da6558', '#849cd5', '#febe48', '#a6cd7a', '#9c6d8e', '#3b545c']
COORDINATES = 'rings-clockwise-30deg-v1'

def ring_label(index):
    if index < 0:
        raise ValueError('Negative ring')
    label = ''
    while True:
        index, letter = divmod(index, 26)
        label = chr(65 + letter) + label
        if index == 0:
            return label
        index -= 1

def split_space(space):
    if ':' in space:
        label, index = space.rsplit(':', 1)
        return label, int(index)
    match = re.fullmatch(r'([A-Z]+)(0|[1-9][0-9]*)', space)
    if not match:
        raise ValueError(f'Invalid space: {space}')
    return match[1], int(match[2])

def ring_index(label):
    value = 0
    for c in label:
        value = value * 26 + ord(c) - 64
    return value - 1

def ring_palette(count):
    return [STANDARD_RING_COLORS[i] if i < len(STANDARD_RING_COLORS)
            else f'hsl({(i * 137) % 360},55%,65%)' for i in range(count)]

def random_ring_palette(rings, identity, players=0):
    """Creation-page palette recipe, using presentation-only randomness."""
    rng = random.Random(identity)
    hue = rng.random()
    band = 0.7 / rings
    palette = []
    for i in range(rings):
        saturation = 0.3 + rng.random() * 0.6
        lightness = 0.1 + band * (rings - 1 - i) + rng.random() * band
        palette.append(f'hsl({hue*360:.3f},{saturation*100:.3f}%,{lightness*100:.3f}%)')
        hue = (hue + rng.random() * 0.4) % 1
    for _ in range(max(0, players-rings)):
        palette.append(f'hsl({rng.random()*360:.3f},{rng.random()*100:.3f}%,{(0.1+rng.random()*0.8)*100:.3f}%)')
    return palette


def player_colors(game):
    if game.get('version') == 2:
        palette = game['board']['ring-colors'] + game['board'].get('palette-tail', [])
        if len(palette) < len(game['players']):
            raise ValueError('Palette does not cover all player seats')
        return dict(zip(game['players'], reversed(palette[:len(game['players'])])))
    return game.get('colors', {})


def load_ogf(path):
    with open(path) as f:
        return json.load(f)


def color_of(game, player):
    """The piece color for a player."""
    return player_colors(game).get(player)


def ring_distances(game):
    """Graph distance of every space from the center (BFS over adjacencies)."""
    if game.get("version") == 2:
        return {s: ring_index(split_space(s)[0]) for s in game["board"]["spaces"]}
    adj = game["board"]["adjacencies"]
    center = game["board"]["center"]
    dist = {center: 0}
    frontier = [center]
    while frontier:
        nxt = []
        for s in frontier:
            for a in adj.get(s, ()):
                if a not in dist:
                    dist[a] = dist[s] + 1
                    nxt.append(a)
        frontier = nxt
    return dist


def layout(game):
    """2D layout {space: (x, y)} matching the concentric hex/pentagon rings: radius =
    ring distance from the center, angle = the cell's index around its ring. Ring r
    has symmetry*r cells, so index n -> angle 2*pi*n/(symmetry*r); notched corner
    cells just leave gaps. This reproduces the board's cell arrangement."""
    if game.get("version") == 2:
        return board_locations(game)
    sym = game.get("symmetry") or 6
    dist = ring_distances(game)
    pos = {}
    for s, d in dist.items():
        if d == 0:
            pos[s] = (0.0, 0.0)
        else:
            n = split_space(s)[1]
            ang = 2 * math.pi * n / (sym * d)
            pos[s] = (d * math.cos(ang), d * math.sin(ang))
    return pos


def track(game, maxd2=3.0):
    """Track element identity across turns so pieces can glide instead of popping.
    Greedy closest-pair matching per (player, type) between consecutive turns; only
    links within maxd2 (squared layout distance) so real (adjacent) moves glide while
    capture-elsewhere + spawn read as separate (pop) rather than a cross-board glide.
    Returns a list of tracks: {player, type, appear, last, path: {turn: [space, food]}}.
    Unmatched new elements start a track (introduce/grow); unmatched live ones end
    (captured/removed)."""
    lay = layout(game)

    def d2(a, b):
        ax, ay = lay[a]; bx, by = lay[b]
        return (ax - bx) ** 2 + (ay - by) ** 2

    tracks, live = [], []
    for i, fr in enumerate(game["frames"]):
        new = {}
        for pl, et, sp, fd in fr["elements"]:
            new.setdefault((pl, et), []).append((sp, fd))
        livebt = {}
        for ti in live:
            t = tracks[ti]
            livebt.setdefault((t["player"], t["type"]), []).append(ti)
        nextlive = []
        for key, items in new.items():
            cand = livebt.get(key, [])
            pairs = []
            for j, (sp, _f) in enumerate(items):
                for ti in cand:
                    dd = d2(sp, tracks[ti]["last_space"])
                    if dd <= maxd2:
                        pairs.append((dd, j, ti))
            pairs.sort()
            usedj, usedt, assign = set(), set(), {}
            for _d, j, ti in pairs:
                if j in usedj or ti in usedt:
                    continue
                usedj.add(j); usedt.add(ti); assign[j] = ti
            for j, (sp, fd) in enumerate(items):
                if j in assign:
                    ti = assign[j]
                    tracks[ti]["path"][i] = [sp, fd]
                    tracks[ti]["last"] = i
                    tracks[ti]["last_space"] = sp
                    nextlive.append(ti)
                else:
                    tracks.append({"player": key[0], "type": key[1], "appear": i, "last": i,
                                   "last_space": sp, "path": {i: [sp, fd]}})
                    nextlive.append(len(tracks) - 1)
        live = nextlive
    return tracks


def board_locations(game):
    """Port of organism.board/board-locations: hexagonal beams from the center
    (phase tau/12) with linearly-interpolated edge cells, indexed [ring:n] to match
    the game's spaces. Returns {space: (x, y)} for the game's spaces (matches the
    printed board art's cell arrangement). Verify with graph-adjacency before use."""
    sym = game.get("symmetry") or 6
    dist = ring_distances(game)
    bydist = {}
    for s in game["board"]["spaces"]:
        bydist[dist[s]] = split_space(s)[0]
    nrings = max(bydist) + 1
    colors = [bydist[d] for d in range(nrings)]          # ring labels, center -> out
    radius = buffer = 1.0
    phase = math.pi / 6.0                                # board.cljc: tau/12
    tau = 2 * math.pi
    off = nrings * radius * buffer

    def axis(i):
        u = (i / sym) * tau + phase
        return (math.cos(u), math.sin(u))

    beams = []
    for i in range(sym):
        ax, ay = axis(i)
        beams.append([(off + ax * r * radius * buffer, off + ay * r * radius * buffer, colors[r])
                      for r in range(nrings)])
    nxt = beams[1:] + beams[:1]
    seq = []                                             # find-rings traversal order
    for sb, eb in zip(beams, nxt):
        seq.extend(sb)                                   # start-beam corner column
        for r in range(nrings):                          # interpolated edge cells per ring
            sx, sy, c = sb[r]; ex, ey, _ = eb[r]
            total = max(r - 1, 0) + 1
            for sp in range(total - 1, 0, -1):
                ratio = sp / total
                seq.append((sx * ratio + ex * (1 - ratio), sy * ratio + ey * (1 - ratio), c))
    pos, cnt = {}, {}
    for (x, y, c) in seq:                                # ring-map + rings->locations indexing
        n = cnt.get(c, 0); cnt[c] = n + 1
        pos.setdefault(f"{c}{n}" if game.get("version") == 2 else f"{c}:{n}", (x - off, y - off))
    return {s: pos[s] for s in game["board"]["spaces"] if s in pos}
