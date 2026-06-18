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

    # ---- #7 MOVE: the MOVE element relocates a FELLOW element (the grower) onto the CENTER
    #      space, "opening" the triangle. Shows what moving is without the actor moving itself
    #      (and without separating the organism). A golden plasma pillar rises under the grower
    #      just before it moves. ----
    #      Triangle: mover back-center (C:1), eater left (B:1), grower right (B:0).
    #      Grower glides B:0 -> A:0 (center, below the mover); organism stays joined via eater B:1.
    #      The plasma is one smooth bell (rise->peak->fall) auto-bracketing the grower's move, peaking
    #      at the move midpoint -- a single gesture overlapping the move, gone before the piece rests.
    "move": {
        "len": 84,
        "cam": {"az": -80, "el": 44, "dist": 255, "lens": 52, "targz": 6, "tx": 0, "ty": 38,
                "drift": {"az": 5, "el": 0.5, "dist": -8}},
        "actors": {"ea": ("green", "eat"), "mv": ("green", "move"), "gr": ("green", "grow")},
        "beats": [
            {"t": 0,  "pos": {"ea": "B:1", "mv": "C:1", "gr": "B:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": []},
            {"t": 8,  "pos": {"ea": "B:1", "mv": "C:1", "gr": "B:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": []},
            {"t": 16, "pos": {"ea": "B:1", "mv": "C:1", "gr": "B:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": ["gr"]},
            {"t": 20, "pos": {"ea": "B:1", "mv": "C:1", "gr": "B:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": ["gr"]},
            {"t": 58, "pos": {"ea": "B:1", "mv": "C:1", "gr": "A:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": ["gr"]},
            {"t": 66, "pos": {"ea": "B:1", "mv": "C:1", "gr": "A:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": []},
            {"t": 84, "pos": {"ea": "B:1", "mv": "C:1", "gr": "A:0"}, "food": {"ea": 1, "mv": 1, "gr": 1}, "glow": []},
        ],
    },

    # ---- #6 EAT: an EAT element eats the food on an adjacent FRONT space (B:4, on the camera
    #      side so it isn't occluded). The plasma flares FIRST and longest (the dramatic beat);
    #      then the bite HOPS in on a continuous arc and lands on the eater; eating also CREATES
    #      a food (the base), so the eater ends with 2 (1 + what was on the space). ----
    "eat": {
        "len": 116,
        "cam": {"az": -34, "el": 50, "dist": 285, "lens": 54, "targz": 6,
                "drift": {"az": -7, "el": -3, "dist": -10}},
        "actors": {"e": ("green", "eat"), "g": ("green", "grow"), "m": ("green", "move")},
        # eater is static, so the plasma bell brackets the glow span (t34..t96 -> bell ~24..106,
        # peak ~65): it starts well before the bite (t44) and outlasts everything (longest curve).
        "beats": [
            {"t": 0,   "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": []},
            {"t": 24,  "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": []},
            {"t": 34,  "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": ["e"]},
            {"t": 96,  "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": ["e"]},
            {"t": 104, "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": []},
            {"t": 116, "pos": {"e": "A:0", "g": "B:1", "m": "B:5"}, "food": {"g": 1, "m": 1}, "glow": []},
        ],
        "food_actors": [
            # eating CREATES a food out of nothing -> grows in EARLY as the BASE (stack 0), during
            # the plasma flare and before the bite descends, so the bite lands on an established food
            {"id": "new",  "k": 0, "keys": [(48, "A:0"), (116, "A:0")], "grow": 20},
            # the board bite (on FRONT space B:4) follows ONE continuous arc: a single high apex
            # (past the B:4->A:0 midpoint) so it descends steeply and lands directly on the created
            # base (stack 1; +6.4=FOOD_DZ). No second key at A:0 -> no separate vertical-drop phase.
            {"id": "bite", "k": 0, "keys": [(0, "B:4"), (44, "B:4"), (62, (9, -15), 56), (82, "A:0", 6.4)]},
        ],
    },

    # ---- #8 GROW: a 5-element organism (2 growers + 2 movers + 1 eater). A single grow spends one
    #      food off EACH grower to grow a NEW (3rd) mover, highlighted by a plasma flare. Viewport
    #      rotated (az=0) so the growers sit on the RIGHT and the new mover grows to their right.
    #      The movers are split: m1 sits up by the left grower, m2 + eater are on the left/center. ----
    "grow": {
        "len": 108,
        "cam": {"az": 0, "el": 50, "dist": 300, "lens": 50, "targz": 6, "tx": -4, "ty": 18,
                "drift": {"az": -5, "el": 0.5, "dist": -8}},
        "actors": {"e": ("green", "eat"), "g1": ("green", "grow"), "g2": ("green", "grow"),
                   "m1": ("green", "move"), "m2": ("green", "move"), "n": ("green", "move")},
        # growers g1(B:0),g2(B:1) each spend 1 food to grow n(C:1, to their right after rotation);
        # m1(B:2) is moved up by left grower g2; m2(B:4) + eater e(A:0) are on the left/center.
        "beats": [
            {"t": 0,   "pos": {"e": "A:0", "g1": "B:0", "g2": "B:1", "m1": "B:2", "m2": "B:4"},            "food": {"e": 1, "g1": 1, "g2": 1}, "glow": []},
            {"t": 30,  "pos": {"e": "A:0", "g1": "B:0", "g2": "B:1", "m1": "B:2", "m2": "B:4"},            "food": {"e": 1, "g1": 1, "g2": 1}, "glow": []},
            {"t": 72,  "pos": {"e": "A:0", "g1": "B:0", "g2": "B:1", "m1": "B:2", "m2": "B:4", "n": "C:1"}, "food": {"e": 1, "g1": 0, "g2": 0}, "glow": ["n"]},
            {"t": 108, "pos": {"e": "A:0", "g1": "B:0", "g2": "B:1", "m1": "B:2", "m2": "B:4", "n": "C:1"}, "food": {"e": 1, "g1": 0, "g2": 0}, "glow": ["n"]},
        ],
    },

    # ---- #9 CIRCULATE: a food stack moves to a NON-ADJACENT element. The eater holds 4 food
    #      (2 carried + 2 on top); the top 2 circulate across to the mover. Two plasma bells in
    #      sequence: first the SOURCE (eater, where it comes from), then the DEST (mover). ----
    "circulate": {
        "len": 104,
        "cam": {"az": -90, "el": 45, "dist": 300, "lens": 50, "targz": 8, "tx": 0, "ty": 0,
                "drift": {"az": 5, "el": 0.5, "dist": -8}},
        "actors": {"e": ("blue", "eat"), "g": ("blue", "grow"), "m": ("blue", "move")},
        # eater e(B:2, left, 4 food) -> mover m(B:5, right), non-adjacent, connected via g(A:0).
        # MUTUAL-RISING timing: food departs (25) as the source bell starts rising (~23), well
        # underway by the peak (43). Reversed at the dest: bell peaks (79) just before the food
        # arrives (85). The food traces a high, centered inverted-U arch (apex over the center at
        # t55) -- a clear up-then-down. source glow [33..53]; dest [69..89].
        "beats": [
            {"t": 0,   "pos": {"e": "B:2", "g": "A:0", "m": "B:5"}, "food": {"e": 2, "g": 0, "m": 0}, "glow": []},
            {"t": 33,  "pos": {"e": "B:2", "g": "A:0", "m": "B:5"}, "food": {"e": 2, "g": 0, "m": 0}, "glow": ["e"]},
            {"t": 53,  "pos": {"e": "B:2", "g": "A:0", "m": "B:5"}, "food": {"e": 2, "g": 0, "m": 0}, "glow": ["e"]},
            {"t": 69,  "pos": {"e": "B:2", "g": "A:0", "m": "B:5"}, "food": {"e": 2, "g": 0, "m": 0}, "glow": ["m"]},
            {"t": 89,  "pos": {"e": "B:2", "g": "A:0", "m": "B:5"}, "food": {"e": 2, "g": 0, "m": 0}, "glow": ["m"]},
            {"t": 104, "pos": {"e": "B:2", "g": "A:0", "m": "B:5"}, "food": {"e": 2, "g": 0, "m": 0}, "glow": []},
        ],
        # the top 2 of the eater's stack arc across (apex over the center grower) onto the mover
        "food_actors": [
            {"id": "c1", "k": 0, "keys": [(0, "B:2"), (25, "B:2"), (55, (0, 0), 68), (85, "B:5")]},
            {"id": "c2", "k": 1, "keys": [(0, "B:2"), (25, "B:2"), (55, (0, 0), 68), (85, "B:5")]},
        ],
    },

    # ---- #10 CONFLICT: a green MOVE element moves into a red organism and captures its EAT
    #      element (move beats eat). First a MOVE (green mover glides adjacent; green highlight),
    #      then the CONFLICT resolves (both elements highlighted in their PLAYER colors), then the
    #      captured red eater is removed and its remains drop as food. Victim centered at A:0;
    #      glow_player_color tints each plasma its player color (green attacker, red victim). ----
    "conflict": {
        "len": 95,
        "glow_player_color": True,
        "cam": {"az": -90, "el": 48, "dist": 300, "lens": 50, "targz": 6, "tx": 0, "ty": -4,
                "drift": {"az": 6, "el": 0.5, "dist": -8}},
        "actors": {"re": ("red", "eat"), "rm": ("red", "move"), "rg": ("red", "grow"),
                   "gm": ("green", "move"), "ge": ("green", "eat"), "gg": ("green", "grow")},
        # red org: victim re(A:0) + rm(B:0) + rg(B:1). green org: gm moves C:8->B:4 (adjacent to re),
        # ge(C:7), gg(C:9). gm glows green ONLY during the move. On contact the victim re flares RED
        # and MORPHS away starting early in that rise (scale 0.9->0 over t40..66 via per-beat "scale");
        # re's carried food FALLS onto its remains: a body food grows at A:0 (stack 0) + the carried
        # token (food_actor) drops onto it (stack 1) = 2 food. (re is omitted from food[] -> its
        # carried token is the food_actor, free to fall.)
        "beats": [
            {"t": 0,   "pos": {"re": "A:0", "rm": "B:0", "rg": "B:1", "gm": "C:8", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "glow": []},
            {"t": 12,  "pos": {"re": "A:0", "rm": "B:0", "rg": "B:1", "gm": "C:8", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "glow": ["gm"]},
            {"t": 38,  "pos": {"re": "A:0", "rm": "B:0", "rg": "B:1", "gm": "B:4", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "glow": ["gm"]},
            {"t": 40,  "pos": {"re": "A:0", "rm": "B:0", "rg": "B:1", "gm": "B:4", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "glow": []},
            {"t": 48,  "pos": {"re": "A:0", "rm": "B:0", "rg": "B:1", "gm": "B:4", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "scale": {"re": 0.5}, "glow": ["re"]},
            {"t": 66,  "pos": {"re": "A:0", "rm": "B:0", "rg": "B:1", "gm": "B:4", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "scale": {"re": 0.0}, "free": {"A:0": 1}, "glow": ["re"]},
            {"t": 72,  "pos": {"rm": "B:0", "rg": "B:1", "gm": "B:4", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "free": {"A:0": 1}, "glow": []},
            {"t": 95,  "pos": {"rm": "B:0", "rg": "B:1", "gm": "B:4", "ge": "C:7", "gg": "C:9"}, "food": {"rm": 1, "rg": 1, "gm": 1, "ge": 1, "gg": 1}, "free": {"A:0": 1}, "glow": []},
        ],
        # re's carried food sits on re (A:0 seat) then FALLS onto the remains (A:0 stack 1) as re morphs
        "food_actors": [
            {"id": "carried", "k": 0, "keys": [(0, "A:0"), (40, "A:0"), (66, (0, 0), 6.4)]},
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
