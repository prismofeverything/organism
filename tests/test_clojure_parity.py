"""Compare Python outcomes with the live Clojure rules, using cached JVM jars.

Run via unittest discovery. Skips when Java or the local Clojure dependencies
are unavailable; it never downloads dependencies or starts the web application.
"""
import json
import os
from pathlib import Path
import shutil
import subprocess
import unittest

from alphazero.games.organism.game import OrganismGame
from alphazero.games.organism import state as gs, choices

ROOT = Path(__file__).resolve().parents[1]
PLAYERS = {'orb', 'mass', 'brone', 'laam', 'stuk'}
ENUMS = {'eat', 'grow', 'move', 'circulate', 'resolve_conflicts', 'check_integrity',
         'center', 'integrity', 'sacrifice'}


def edn(value, key=False):
    if value is None:
        return 'nil'
    if isinstance(value, bool):
        return 'true' if value else 'false'
    if isinstance(value, dict):
        return '{' + ' '.join(edn(k, True) + ' ' + edn(v) for k, v in value.items()) + '}'
    if isinstance(value, (tuple, list)):
        return '[' + ' '.join(edn(v) for v in value) + ']'
    if isinstance(value, str):
        if (key and value not in PLAYERS) or value in ENUMS:
            return ':' + value.replace('_', '-')
        return json.dumps(value)
    return str(value)


def snapshot(game):
    state = game['state']
    result = {
        'elements': [{k: e[k] for k in ('player', 'type', 'space', 'food')}
                     for _, e in sorted(state['elements'].items())],
        'food': sorted(state['food'].items()),
        'captures': {p: sorted([{k: c[k] for k in ('player', 'type')} for c in captures],
                              key=lambda c: (c['player'], c['type']))
                     for p, captures in state['captures'].items()},
        'winner': state['winner'],
    }
    return json.loads(json.dumps(result))


class ClojureParityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        java = shutil.which('java')
        cache = Path.home() / '.m2/repository'
        artifacts = ['org/clojure/clojure/1.12.0/clojure-1.12.0.jar',
                     'org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar',
                     'org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar',
                     'org/clojure/math.combinatorics/0.1.6/math.combinatorics-0.1.6.jar',
                     'org/clojure/data.json/2.5.1/data.json-2.5.1.jar',
                     'com/cgore/mersenne-twister/1.0.0/mersenne-twister-1.0.0.jar',
                     'org/apache/commons/commons-math3/3.6.1/commons-math3-3.6.1.jar']
        if not java or not all((cache / a).exists() for a in artifacts):
            raise unittest.SkipTest('Java/Clojure oracle dependencies are not cached')
        cls.command = [java, '-Xmx256m', '-cp', os.pathsep.join(
            [str(ROOT / 'src/cljc'), str(ROOT / 'test/clj')] + [str(cache / a) for a in artifacts]),
            'clojure.main', str(ROOT / 'tests/clojure_rule_oracle.clj')]

    def compare(self, requests, expected):
        # Operation values are keywords too; enum conversion is scoped here.
        payload = '[' + ' '.join('{:op :' + r['op'].replace('_', '-') + ' ' +
                                ' '.join(edn(k, True) + ' ' + edn(v)
                                         for k, v in r.items() if k != 'op') + '}'
                                for r in requests) + ']'
        result = subprocess.run(self.command, input=payload, text=True, capture_output=True,
                                cwd=ROOT, timeout=45)
        self.assertEqual(result.returncode, 0, result.stderr)
        actual = json.loads(result.stdout)
        for index, (got, want) in enumerate(zip(actual, expected)):
            with self.subTest(case=index, op=requests[index]['op']):
                self.assertEqual(got, want)
        self.assertEqual(len(actual), len(expected))

    def test_base_actions_and_board_geometry(self):
        requests, expected = [], []
        game = OrganismGame(2, num_rings=4, remove_notches=False)
        for food in (0, 1, 2, 3, 5, 10, 111):
            state = game.initial_state()
            state = gs.add_element(state, 'orb', 0, 'eat', (1, 0), food)
            state = gs.add_element(state, 'orb', 0, 'move', (1, 1), 4)
            fields = {'from': (1, 0), 'to': (1, 1)}
            requests.append(dict(op='circulate', game=state, fields=fields))
            expected.append(snapshot(gs.circulate_action(state, fields)))
        for symmetry, rings, notches in [(6, 4, False), (6, 7, True), (5, 7, True)]:
            from alphazero.games.organism.board import build_board
            _, adjacent, _, _ = build_board(symmetry, rings, notches)
            requests.append(dict(op='board', symmetry=symmetry, rings=rings, notches=notches))
            expected.append(json.loads(json.dumps([[s, sorted(a)] for s, a in sorted(adjacent.items())])))
        state = game.initial_state()
        starts = game._player_info[0][1]['starting_spaces']
        neighbor = next(s for s in gs.surrounding_spaces(state, starts) if s not in starts)
        state['state']['food'] = {starts[0]: 7, neighbor: 4}
        fields = {'organism': 0, 'spaces': dict(zip(starts, ['eat', 'grow', 'move']))}
        requests.append(dict(op='introduce', game=state, player='orb', fields=fields))
        expected.append(snapshot(gs.introduce_spaces(state, 'orb', fields)))
        state = gs.introduce_spaces(state, 'orb', fields)
        state = gs.choose_organism_action(state, 0)
        state = gs.choose_action_type_action(state, 'grow')
        fields = {'element': 'eat', 'from': {starts[1]: 1}, 'to': (2, 0)}
        state['state']['food'][(2, 0)] = 6
        requests.append(dict(op='grow', game=state, fields=fields))
        expected.append(snapshot(gs.grow_action(state, fields)))
        fields = {'from': starts[2], 'to': (2, 0)}
        requests.append(dict(op='move', game=state, fields=fields))
        expected.append(snapshot(gs.move_action(state, fields)))
        fields = {'from': (2, 0), 'to': starts[0]}
        requests.append(dict(op='eat', game=state, fields=fields))
        expected.append(snapshot(gs.eat_action(state, fields)))
        self.compare(requests, expected)

    def test_scoring_and_legal_choices(self):
        import random
        requests, expected = [], []
        game = OrganismGame(3, num_rings=4, remove_notches=False)
        state = game.initial_state()
        for player, kind, space in [('orb', 'eat', (1, 0)), ('mass', 'grow', (1, 1)),
                                     ('brone', 'move', (1, 2))]:
            state = gs.add_element(state, player, 0, kind, space, 1)
        requests.append(dict(op='resolve_conflicts', game=state, player='mass'))
        expected.append(snapshot(gs.resolve_conflicts(state, 'mass')))
        state = game.initial_state()
        for space in [(1, 0), (3, 9)]:
            state = gs.add_element(state, 'mass', 0, 'eat', space, 1)
        requests.append(dict(op='check_integrity', game=state, player='orb'))
        expected.append(snapshot(gs.check_integrity(state, 'orb')))
        state = game.initial_state()
        state = gs.add_element(state, 'orb', 0, 'eat', (1, 0), 1)
        state['state']['elements'][(1, 0)]['captures'] = [{'player': 'mass'}]
        requests.append(dict(op='check_integrity', game=state, player='orb'))
        expected.append(snapshot(gs.check_integrity(state, 'orb')))
        for counts in [(5, 5, 0), (5, 5, 5), (5, 5, 6)]:
            state = game.initial_state()
            state['state']['captures'] = {p: [{}] * count for p, count in zip(state['turn_order'], counts)}
            requests.append(dict(op='victory', game=state))
            expected.append(gs.victory(state))
        rng = random.Random(42)
        for _ in range(20):
            state = game.initial_state()
            spaces = rng.sample(game.all_spaces, 15)
            for i, space in enumerate(spaces):
                state = gs.add_element(state, 'orb' if i == 0 else rng.choice(state['turn_order']),
                                       0, rng.choice(['eat', 'grow', 'move']), space,
                                       rng.choice([0, 1, 2, 5, 111]))
            state = gs.find_organisms(state)
            organism, owned = next(iter(gs.player_organisms(state, 'orb').items()))
            state = gs.choose_organism_action(state, organism)
            requests.append(dict(op='legal_probes', game=state))
            elements = [e for _, e in sorted(state['state']['elements'].items())]
            probes = {'eat': [(e['space'], gs.can_eat(state, e)) for e in elements],
                      'move': [(e['space'], gs.can_move(state, e['space'])) for e in elements],
                      'destinations': [(e['space'], sorted(gs.available_spaces(state, e['space']))) for e in elements],
                      'growth': sorted(gs.growable_spaces(state, [e['space'] for e in owned if e['type'] == 'grow']))}
            expected.append(json.loads(json.dumps(probes)))
        state = game.initial_state()
        starts = game._player_info[0][1]['starting_spaces']
        state = gs.introduce_spaces(state, 'orb', {'organism': 0, 'spaces': dict(zip(starts, ['eat', 'grow', 'move']))})
        state['state']['elements'][starts[0]]['food'] = 111
        state = gs.choose_organism_action(state, 0)
        state = gs.choose_action_type_action(state, 'eat')
        requests.append(dict(op='action_choices', game=state, type='eat'))
        options = choices._choose_action_choices(state, 'eat')
        expected.append(sorted(s['state']['player_turn']['organism_turns'][-1]['actions'][-1]['type']
                               if not s['state']['player_turn']['organism_turns'][-1]['actions'][-1]['action'].get('pass')
                               else 'pass' for s in options.values()))
        state = gs.choose_action_action(state, 'circulate')
        state = gs.set_action_field(state, 'from', starts[1])
        requests.append(dict(op='circulate_choices', game=state))
        expected.append([list(game.idx_to_space[i]) for i in sorted(choices._circulate_to_choices(state, gs.player_organisms(state, 'orb')[0]))])
        self.compare(requests, expected)
