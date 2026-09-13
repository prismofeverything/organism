import math
import unittest
from alphazero.games.organism.board import build_rings, find_adjacencies
from pieces.organism_format import (ring_label, ring_index, split_space, ring_palette,
    board_locations, player_colors, COORDINATES)

class RingCoordinates(unittest.TestCase):
    def test_letters_beyond_z_and_legacy_ids(self):
        for n in [0,1,25,26,27,51,52,701,702]:
            self.assertEqual(ring_index(ring_label(n)), n)
        self.assertEqual(ring_label(26), 'AA')
        self.assertEqual(split_space('AA12'), ('AA',12))
        self.assertEqual(split_space('dark:red:12'), ('dark:red',12))

    def test_common_axis_clockwise_and_topology_for_five_six_seven(self):
        for symmetry in [5,6,7]:
            rings=build_rings(symmetry,5);adj=find_adjacencies(rings)
            encode=lambda s:f'{ring_label(s[0])}{s[1]}'
            g={'version':2,'symmetry':symmetry,'players':['a','b','c'],
               'board':{'center':'A0','coordinates':COORDINATES,'ring-colors':ring_palette(5),
                        'spaces':[encode(s) for s in adj],
                        'adjacencies':{encode(s):[encode(a) for a in aa] for s,aa in adj.items()}}}
            pos=board_locations(g)
            self.assertEqual(set(pos),set(g['board']['spaces']))
            self.assertEqual(pos['A0'],(0.,0.))
            for r in range(1,5):
                x,y=pos[f'{ring_label(r)}0']
                self.assertAlmostEqual(math.atan2(y,x),math.pi/6)
                previous=math.pi/6
                for i in range(1,symmetry*r):
                    x,y=pos[f'{ring_label(r)}{i}'];angle=math.atan2(y,x)
                    while angle<math.pi/6:angle+=2*math.pi
                    self.assertGreater(angle,previous);previous=angle
            original=dict(pos)
            g['board']['ring-colors']=['#123456']*5
            self.assertEqual(board_locations(g),original)
            self.assertEqual(player_colors(g),dict.fromkeys(g['players'],'#123456'))
            # Missing/notched outer spaces keep their original indices.
            g['board']['spaces'].remove('E0')
            self.assertNotIn('E0',board_locations(g))
            self.assertEqual(board_locations(g)['E1'],original['E1'])

    def test_player_colors_match_site_reverse_first_seat_count(self):
        g={'version':2,'players':['one','two','three'],'board':{'ring-colors':['yellow','red','blue','orange']}}
        self.assertEqual(player_colors(g),{'one':'blue','two':'red','three':'yellow'})
        g['board']={'ring-colors':['yellow','red'],'palette-tail':['blue']}
        self.assertEqual(player_colors(g),{'one':'blue','two':'red','three':'yellow'})
