"""Storyboards for build_clip.py — one entry per short clip.

Spaces are real board ids. Central adjacencies used to author valid states:
  A:0 -> B:0..B:5
  B:0 -> A:0,B:1,B:5,C:0,C:1,C:11   B:1 -> A:0,B:0,B:2,C:1,C:2,C:3
  B:2 -> A:0,B:1,B:3,C:3,C:4,C:5    B:3 -> A:0,B:2,B:4,C:5,C:6,C:7
  B:4 -> A:0,B:3,B:5,C:7,C:8,C:9    B:5 -> A:0,B:4,B:0,C:9,C:10,C:11
  C:7 -> C:6,C:8,B:3,B:4,D:10,D:11
A beat is a state the board passes through; the engine glides between beats.
  beat = {"t": frame, "pos": {id: space}, "food": {id: n}, "free": {space: n}, "glow": [id]}
  actors = {id: (color, type)}   color in green|red|purple|blue|yellow|dark ; type eat|move|grow
  food_actors = [{"id":.., "k":stack_idx, "keys":[(t, space), ...]}]   # a token that glides
  cam = {az, el, dist, lens, targz, drift:{az,el,dist,targz}}  (deg / mm)
  from_ogf = <turn> builds a static state from the recorded game (board/glow shots);
             glow_space highlights one element and frames the camera on it.
"""

STORY = {

    # ---- #7 MOVE: a fed element glides to an adjacent space ----
    "move": {
        "len": 120,
        "cam": {"az": 40, "el": 42, "dist": 300, "lens": 52, "targz": 6,
                "drift": {"az": 8, "el": 1.5, "dist": -14}},
        "actors": {"e": ("green", "eat"), "g": ("green", "grow"), "m": ("green", "move")},
        "beats": [
            {"t": 0,   "pos": {"e": "A:0", "g": "B:1", "m": "B:0"}, "food": {"e": 1, "g": 1, "m": 1}, "glow": ["m"]},
            {"t": 45,  "pos": {"e": "A:0", "g": "B:1", "m": "B:0"}, "food": {"e": 1, "g": 1, "m": 1}, "glow": ["m"]},
            {"t": 95,  "pos": {"e": "A:0", "g": "B:1", "m": "C:0"}, "food": {"e": 1, "g": 1, "m": 1}, "glow": ["m"]},
            {"t": 120, "pos": {"e": "A:0", "g": "B:1", "m": "C:0"}, "food": {"e": 1, "g": 1, "m": 1}, "glow": ["m"]},
        ],
    },

    # ---- #6 EAT: a food token slides from the board onto an adjacent EAT element ----
    "eat": {
        "len": 120,
        "cam": {"az": -34, "el": 50, "dist": 285, "lens": 54, "targz": 6,
                "drift": {"az": -7, "el": -3, "dist": -10}},
        "actors": {"e": ("green", "eat"), "g": ("green", "grow"), "m": ("green", "move")},
        "beats": [
            {"t": 0,   "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": ["e"]},
            {"t": 40,  "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": ["e"]},
            {"t": 85,  "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": ["e"]},
            {"t": 120, "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": ["e"]},
        ],
        "food_actors": [{"id": "bite", "keys": [(0, "B:0"), (40, "B:0"), (85, "A:0"), (120, "A:0")]}],
    },

    # ---- #8 GROW: two growers spend a food each; a new element appears by a grower ----
    "grow": {
        "len": 124,
        "cam": {"az": -22, "el": 47, "dist": 300, "lens": 52, "targz": 6,
                "drift": {"az": -9, "el": 1, "dist": -12}},
        "actors": {"e": ("green", "eat"), "m": ("green", "move"),
                   "g1": ("green", "grow"), "g2": ("green", "grow"), "n": ("green", "grow")},
        "beats": [
            {"t": 0,   "pos": {"e": "A:0", "m": "B:2", "g1": "B:0", "g2": "B:1"},            "food": {"e": 1, "m": 1, "g1": 1, "g2": 1}, "glow": ["g1", "g2"]},
            {"t": 42,  "pos": {"e": "A:0", "m": "B:2", "g1": "B:0", "g2": "B:1"},            "food": {"e": 1, "m": 1, "g1": 1, "g2": 1}, "glow": ["g1", "g2"]},
            {"t": 82,  "pos": {"e": "A:0", "m": "B:2", "g1": "B:0", "g2": "B:1", "n": "C:0"}, "food": {"e": 1, "m": 1, "g1": 0, "g2": 0}, "glow": ["n"]},
            {"t": 124, "pos": {"e": "A:0", "m": "B:2", "g1": "B:0", "g2": "B:1", "n": "C:0"}, "food": {"e": 1, "m": 1, "g1": 0, "g2": 0}, "glow": ["n"]},
        ],
    },

    # ---- #9 CIRCULATE: half a food stack moves to a non-adjacent element of the organism ----
    "circulate": {
        "len": 126,
        "cam": {"az": 18, "el": 43, "dist": 330, "lens": 50, "targz": 6,
                "drift": {"az": 9, "el": 2, "dist": -12}},
        "actors": {"e": ("blue", "eat"), "m": ("blue", "move"),
                   "src": ("blue", "grow"), "dst": ("blue", "grow")},
        "beats": [
            {"t": 0,   "pos": {"e": "B:1", "m": "A:0", "src": "B:0", "dst": "B:2"}, "food": {"e": 1, "m": 1, "src": 2, "dst": 0}, "glow": ["src", "dst"]},
            {"t": 45,  "pos": {"e": "B:1", "m": "A:0", "src": "B:0", "dst": "B:2"}, "food": {"e": 1, "m": 1, "src": 2, "dst": 0}, "glow": ["src", "dst"]},
            {"t": 95,  "pos": {"e": "B:1", "m": "A:0", "src": "B:0", "dst": "B:2"}, "food": {"e": 1, "m": 1, "src": 2, "dst": 0}, "glow": ["src", "dst"]},
            {"t": 126, "pos": {"e": "B:1", "m": "A:0", "src": "B:0", "dst": "B:2"}, "food": {"e": 1, "m": 1, "src": 2, "dst": 0}, "glow": ["src", "dst"]},
        ],
        # two tokens off the top of src's stack glide across to dst
        "food_actors": [
            {"id": "c1", "k": 0, "keys": [(0, "B:0"), (45, "B:0"), (95, "B:2"), (126, "B:2")]},
            {"id": "c2", "k": 1, "keys": [(0, "B:0"), (45, "B:0"), (95, "B:2"), (126, "B:2")]},
        ],
    },

    # ---- #10 CONFLICT: adjacent enemies resolve (move beats eat); loser -> free food ----
    "conflict": {
        "len": 130,
        "cam": {"az": 56, "el": 40, "dist": 292, "lens": 54, "targz": 6,
                "drift": {"az": 11, "el": -2, "dist": -8}},
        "actors": {"gm": ("green", "move"), "ge": ("green", "eat"), "gg": ("green", "grow"),
                   "re": ("red", "eat"), "rm": ("red", "move"), "rg": ("red", "grow")},
        "beats": [
            {"t": 0,   "pos": {"gm": "A:0", "ge": "B:2", "gg": "B:3", "re": "B:0", "rm": "C:0", "rg": "C:1"}, "food": {"gm": 1, "ge": 1, "gg": 1, "re": 1, "rm": 1, "rg": 1}, "glow": ["gm", "re"]},
            {"t": 52,  "pos": {"gm": "A:0", "ge": "B:2", "gg": "B:3", "re": "B:0", "rm": "C:0", "rg": "C:1"}, "food": {"gm": 1, "ge": 1, "gg": 1, "re": 1, "rm": 1, "rg": 1}, "glow": ["gm", "re"]},
            {"t": 74,  "pos": {"gm": "A:0", "ge": "B:2", "gg": "B:3", "rm": "C:0", "rg": "C:1"},              "food": {"gm": 1, "ge": 1, "gg": 1, "rm": 1, "rg": 1}, "free": {"B:0": 2}, "glow": ["gm"]},
            {"t": 130, "pos": {"gm": "A:0", "ge": "B:2", "gg": "B:3", "rm": "C:0", "rg": "C:1"},              "food": {"gm": 1, "ge": 1, "gg": 1, "rm": 1, "rg": 1}, "free": {"B:0": 2}, "glow": []},
        ],
    },

    # ---- #11 PERISH: an organism with no EAT dies; its elements become piles of food ----
    "perish": {
        "len": 120,
        "cam": {"az": -42, "el": 45, "dist": 300, "lens": 52, "targz": 6,
                "drift": {"az": -7, "el": 3, "dist": -8}},
        "actors": {"m": ("purple", "move"), "g1": ("purple", "grow"), "g2": ("purple", "grow")},
        "beats": [
            {"t": 0,   "pos": {"m": "A:0", "g1": "B:0", "g2": "B:1"}, "food": {"m": 1, "g1": 1, "g2": 1}, "glow": ["m", "g1", "g2"]},
            {"t": 44,  "pos": {"m": "A:0", "g1": "B:0", "g2": "B:1"}, "food": {"m": 1, "g1": 1, "g2": 1}, "glow": ["m", "g1", "g2"]},
            {"t": 72,  "pos": {}, "food": {}, "free": {"A:0": 2, "B:0": 2, "B:1": 2}, "glow": []},
            {"t": 120, "pos": {}, "food": {}, "free": {"A:0": 2, "B:0": 2, "B:1": 2}, "glow": []},
        ],
    },

    # ---- #5 TWO ORGANISMS: one player acts in each (highlight one, then the other) ----
    "two_org": {
        "len": 144,
        "cam": {"az": 4, "el": 53, "dist": 420, "lens": 50, "targz": 5,
                "drift": {"az": 7, "el": -1.5, "dist": -10}},
        "actors": {"e1": ("green", "eat"), "m1": ("green", "move"), "g1": ("green", "grow"),
                   "e2": ("green", "eat"), "m2": ("green", "move"), "g2": ("green", "grow")},
        "_pos": "org1 around B:1 (top), org2 around B:4 (opposite) -> separate organisms",
        "beats": [
            {"t": 0,   "pos": {"e1": "B:1", "m1": "C:2", "g1": "C:3", "e2": "B:4", "m2": "C:8", "g2": "C:7"}, "food": {"e1": 1, "m1": 1, "g1": 1, "e2": 1, "m2": 1, "g2": 1}, "glow": []},
            {"t": 22,  "pos": {"e1": "B:1", "m1": "C:2", "g1": "C:3", "e2": "B:4", "m2": "C:8", "g2": "C:7"}, "food": {"e1": 1, "m1": 1, "g1": 1, "e2": 1, "m2": 1, "g2": 1}, "glow": ["m1"]},
            {"t": 64,  "pos": {"e1": "B:1", "m1": "C:2", "g1": "C:3", "e2": "B:4", "m2": "C:8", "g2": "C:7"}, "food": {"e1": 1, "m1": 1, "g1": 1, "e2": 1, "m2": 1, "g2": 1}, "glow": ["m1"]},
            {"t": 84,  "pos": {"e1": "B:1", "m1": "C:2", "g1": "C:3", "e2": "B:4", "m2": "C:8", "g2": "C:7"}, "food": {"e1": 1, "m1": 1, "g1": 1, "e2": 1, "m2": 1, "g2": 1}, "glow": ["g2"]},
            {"t": 126, "pos": {"e1": "B:1", "m1": "C:2", "g1": "C:3", "e2": "B:4", "m2": "C:8", "g2": "C:7"}, "food": {"e1": 1, "m1": 1, "g1": 1, "e2": 1, "m2": 1, "g2": 1}, "glow": ["g2"]},
            {"t": 144, "pos": {"e1": "B:1", "m1": "C:2", "g1": "C:3", "e2": "B:4", "m2": "C:8", "g2": "C:7"}, "food": {"e1": 1, "m1": 1, "g1": 1, "e2": 1, "m2": 1, "g2": 1}, "glow": ["g2"]},
        ],
    },

    # ---- #3 THREE ORGANISMS: a last element glides in to complete the third (then all glow) ----
    "three_org": {
        "len": 150,
        "cam": {"az": -8, "el": 56, "dist": 450, "lens": 50, "targz": 5,
                "drift": {"az": 6, "el": -2, "dist": -12}},
        "actors": {
            "ae": ("green", "eat"), "am": ("green", "move"), "ag": ("green", "grow"),   # org A (top-right)
            "be": ("green", "eat"), "bm": ("green", "move"), "bg": ("green", "grow"),   # org B (bottom)
            "ce": ("green", "eat"), "cg": ("green", "grow"), "last": ("green", "move"), # org C (left) + last
        },
        # org A: B:1,C:2,C:3 | org B: B:3,C:6,C:5 | org C: B:5,C:10 + last glides C:8->C:9
        "beats": [
            {"t": 0,   "pos": {"ae": "B:1", "am": "C:2", "ag": "C:3", "be": "B:3", "bm": "C:6", "bg": "C:5", "ce": "B:5", "cg": "C:10", "last": "C:8"},
             "food": {k: 1 for k in ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]}, "glow": ["last"]},
            {"t": 44,  "pos": {"ae": "B:1", "am": "C:2", "ag": "C:3", "be": "B:3", "bm": "C:6", "bg": "C:5", "ce": "B:5", "cg": "C:10", "last": "C:8"},
             "food": {k: 1 for k in ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]}, "glow": ["last"]},
            {"t": 92,  "pos": {"ae": "B:1", "am": "C:2", "ag": "C:3", "be": "B:3", "bm": "C:6", "bg": "C:5", "ce": "B:5", "cg": "C:10", "last": "C:9"},
             "food": {k: 1 for k in ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]}, "glow": ["last"]},
            {"t": 112, "pos": {"ae": "B:1", "am": "C:2", "ag": "C:3", "be": "B:3", "bm": "C:6", "bg": "C:5", "ce": "B:5", "cg": "C:10", "last": "C:9"},
             "food": {k: 1 for k in ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]},
             "glow": ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]},
            {"t": 150, "pos": {"ae": "B:1", "am": "C:2", "ag": "C:3", "be": "B:3", "bm": "C:6", "bg": "C:5", "ce": "B:5", "cg": "C:10", "last": "C:9"},
             "food": {k: 1 for k in ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]},
             "glow": ["ae", "am", "ag", "be", "bm", "bg", "ce", "cg", "last"]},
        ],
    },

    # ---- #2 FULL BOARD WITH PIECES: a real busy game state, slow drift ----
    "board": {
        "from_ogf": 144, "len": 120,
        "cam": {"az": 26, "el": 50, "dist": 470, "lens": 48, "targz": 5,
                "drift": {"az": 8, "el": -2, "dist": -14}},
    },

    # ---- #12 ELEMENT GLOW: same board, one element highlighted (camera framed on it) ----
    "glow": {
        "from_ogf": 144, "len": 100, "glow_space": "B:0",
        "cam": {"az": 30, "el": 44, "dist": 300, "lens": 52, "targz": 5,
                "drift": {"az": 7, "el": 1, "dist": -8}},
    },

}
