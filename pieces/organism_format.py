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


def load_ogf(path):
    with open(path) as f:
        return json.load(f)


def color_of(game, player):
    """The piece color for a player."""
    return game["colors"].get(player)


def ring_distances(game):
    """Graph distance of every space from the center (BFS over adjacencies)."""
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
    """A concentric 2D layout {space: (x, y)} derived from the board graph: ring =
    graph distance from center, spaces spread evenly around each ring. Good enough
    for animation; swap in organism.board/board-locations to match the printed art."""
    dist = ring_distances(game)
    rings = {}
    for s, d in dist.items():
        rings.setdefault(d, []).append(s)
    pos = {}
    for d, spaces in rings.items():
        spaces.sort()
        k = max(len(spaces), 1)
        for i, s in enumerate(spaces):
            ang = 2 * math.pi * i / k
            pos[s] = (d * math.cos(ang), d * math.sin(ang))
    return pos
