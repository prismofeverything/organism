"""Game-port regressions that run without installing PyTorch."""
import copy
import unittest

from alphazero.games.organism.game import OrganismGame
from alphazero.games.organism import choices, state as gs


class RulesTests(unittest.TestCase):
    def test_automatic_fast_path_preserves_full_transition_results(self):
        import random
        rng = random.Random(9)
        automatic_count = 0
        for players in (2, 3):
            game = OrganismGame(players, num_rings=4, remove_notches=False)
            state = game.initial_state()
            for _ in range(250):
                _, full = choices.find_state(state)
                _, fast = choices.find_state(state, automatic_only=True)
                if full and all(k < 0 for k in full):
                    self.assertEqual(fast, full)
                    automatic_count += 1
                else:
                    self.assertEqual(fast, {})
                if not full:
                    break
                state = full[rng.choice(list(full))]
        self.assertGreater(automatic_count, 10)

    def test_evenly_spaced_setups(self):
        expected = {2: [2, 20], 3: [2, 14, 26]}
        for players, starts in expected.items():
            game = OrganismGame(players)
            self.assertEqual([info['starting_spaces'][0][1] for _, info in game._player_info], starts)
            self.assertEqual(game.symmetry, 6)
            tensor = game.encode_state(game.initial_state(), game._turn_order[0])
            self.assertEqual(tensor.shape[1:], (36, 36))

    def test_simulation_branches_do_not_mutate_each_other(self):
        game = OrganismGame(3)
        state = game.initial_state()
        before = copy.deepcopy(state)
        children = list(game.legal_actions(state).values())
        self.assertEqual(state, before)
        first_space = next(iter(children[0]['state']['elements']))
        children[0]['state']['elements'][first_space]['food'] = 999
        self.assertNotEqual(children[1]['state']['elements'][first_space]['food'], 999)
        self.assertEqual(state, before)

    def test_terminal_automatic_transition_preserves_winner(self):
        game = OrganismGame(2)
        state = game.initial_state()
        state['state']['captures']['orb'] = [{}] * 5
        terminal = game._advance_automatic(state)
        self.assertTrue(game.is_terminal(terminal))
        self.assertIsNone(game.current_player(terminal))
        self.assertEqual(game.rewards(terminal), {'orb': 1, 'mass': -1})
        self.assertFalse(game.is_terminal(state))

    def test_introduction_clears_home_food_but_preserves_adjacent_food(self):
        game = OrganismGame(2, num_rings=4, remove_notches=False)
        state = game.initial_state()
        starts = game._player_info[0][1]['starting_spaces']
        adjacent = next(s for s in gs.surrounding_spaces(state, starts) if s not in starts)
        state['state']['food'] = {**dict.fromkeys(starts, 9), adjacent: 6}
        state = gs.add_element(state, 'mass', 1, 'eat', adjacent, 4)
        fields = {'organism': 0, 'spaces': dict(zip(starts, ['eat', 'grow', 'move']))}
        introduced = gs.introduce_spaces(state, 'orb', fields)
        self.assertEqual(introduced['state']['food'], {adjacent: 6})
        self.assertTrue(all(e['food'] == 1 for e in introduced['state']['elements'].values()))
        self.assertNotIn(adjacent, introduced['state']['elements'])
        self.assertEqual(state['state']['food'][starts[0]], 9)

    def test_network_can_distinguish_large_food_stores(self):
        game = OrganismGame(2)
        state = gs.add_element(game.initial_state(), 'orb', 0, 'eat', (1, 0), 11)
        state['state']['food'][(1, 1)] = 6
        encoded = game.encode_state(state, 'orb')
        state['state']['elements'][(1, 0)]['food'] = 111
        self.assertTrue((encoded != game.encode_state(state, 'orb')).any())
        state['state']['elements'][(1, 0)]['food'] = 11
        state['state']['food'][(1, 1)] = 60
        self.assertTrue((encoded != game.encode_state(state, 'orb')).any())

    def test_circulation_moves_half_rounded_up(self):
        game = OrganismGame(2)
        for amount in (0, 1, 2, 3, 5, 10, 111):
            state = game.initial_state()
            state = gs.add_element(state, 'orb', 0, 'eat', (1, 0), amount)
            state = gs.add_element(state, 'orb', 0, 'move', (1, 1), 4)
            result = gs.circulate_action(state, {'from': (1, 0), 'to': (1, 1)})
            moved = (amount + 1) // 2
            self.assertEqual(result['state']['elements'][(1, 0)]['food'], amount - moved)
            self.assertEqual(result['state']['elements'][(1, 1)]['food'], 4 + moved)

    def test_capture_chain_resolves_before_captor_is_removed(self):
        game = OrganismGame(3)
        state = game.initial_state()
        for player, kind, space in [('orb', 'eat', (1, 0)), ('mass', 'grow', (1, 1)),
                                     ('brone', 'move', (1, 2))]:
            state = gs.add_element(state, player, 0, kind, space, 1)
        resolved = gs.resolve_conflicts(state, 'mass')
        self.assertEqual([c['player'] for c in resolved['state']['captures']['orb']], ['mass'])
        self.assertEqual([c['player'] for c in resolved['state']['captures']['mass']], ['brone'])
        self.assertEqual(set(resolved['state']['elements']), {(1, 0)})
        self.assertEqual(len(state['state']['elements']), 3)

    def test_integrity_scores_opponents_and_sacrifices(self):
        game = OrganismGame(2)
        state = game.initial_state()
        state = gs.add_element(state, 'mass', 0, 'eat', (1, 0), 1)
        state = gs.add_element(state, 'mass', 1, 'eat', (3, 9), 1)
        resolved = gs.check_integrity(state, 'orb')
        self.assertEqual(len(resolved['state']['captures']['orb']), 1)
        state = game.initial_state()
        state = gs.add_element(state, 'orb', 0, 'eat', (1, 0), 1)
        state['state']['elements'][(1, 0)]['captures'] = [{'player': 'mass'}]
        resolved = gs.check_integrity(state, 'orb')
        self.assertEqual(resolved['state']['captures']['mass'][0]['type'], 'sacrifice')

    def test_repetition_distinguishes_resources_and_decisions(self):
        game = OrganismGame(2)
        state = game.initial_state()
        key = game.repetition_key(state)
        state['state']['round'] += 1
        self.assertEqual(key, game.repetition_key(state))
        for field, value in [('food', {(0, 0): 1}), ('az_grow_from', {(1, 0): 1})]:
            other = copy.deepcopy(state)
            other['state'][field] = value
            self.assertNotEqual(key, game.repetition_key(other))
        state['state']['player_turn']['player'] = 'mass'
        self.assertNotEqual(key, game.repetition_key(state))

    def test_capture_ties_follow_current_game_rules(self):
        game = OrganismGame(3)
        state = game.initial_state()
        state['state']['captures'] = {'orb': [{}] * 5, 'mass': [{}] * 5, 'brone': []}
        self.assertEqual(gs.victory(state), 'mass')  # acting orb caused the tie
        state['state']['captures']['brone'] = [{}] * 5
        self.assertIsNone(gs.victory(state))  # two non-acting leaders remain
        state['state']['captures']['brone'].append({})
        self.assertEqual(gs.victory(state), 'brone')

    def test_organism_ids_do_not_collide_modulo_ten(self):
        game = OrganismGame(2)
        state = game.initial_state()
        for space, org in [((1, 0), 0), ((2, 4), 10)]:
            state = gs.add_element(state, 'orb', org, 'eat', space, 1)
        options = choices._choose_organism_choices(state, [0, 10])
        self.assertEqual(len(options), 2)
        self.assertEqual({s['state']['player_turn']['organism_turns'][-1]['organism']
                          for s in options.values()}, {0, 10})

    def test_growth_can_allocate_more_than_ten_distinct_ways(self):
        game = OrganismGame(2)
        state = game.initial_state()
        for step in range(4):
            state = gs.add_element(state, 'orb', 0, 'grow', (3, step), 3)
        for step in range(3):
            state = gs.add_element(state, 'orb', 0, 'eat', (2, step), 1)
        state = gs.choose_organism_action(state, 0)
        state = gs.choose_action_type_action(state, 'grow')
        state = gs.choose_action_action(state, 'grow')
        state = gs.set_action_field(state, 'element', 'eat')
        pending = [state]
        allocations = set()
        while pending:
            node = pending.pop()
            fields = node['state']['player_turn']['organism_turns'][-1]['actions'][-1]['action']
            if 'from' in fields:
                self.assertNotIn('az_grow_from', node['state'])
                allocations.add(tuple(sorted(fields['from'].items())))
            else:
                elements = gs.player_organisms(node, 'orb')[0]
                pending.extend(choices._grow_from_choices(node, elements).values())
        self.assertEqual(len(allocations), 20)  # compositions of 3 across 4 donors

    def test_automatic_turns_return_the_new_player(self):
        game = OrganismGame(2)
        state = game.initial_state()
        seen = set()
        for _ in range(50):
            player = game.current_player(state)
            seen.add(player)
            state = next(iter(game.legal_actions(state).values()))
            self.assertNotIn(state['state']['player_turn']['advance'],
                             ['resolve_conflicts', 'check_integrity'])
            if len(seen) == 2:
                break
        self.assertEqual(seen, {'orb', 'mass'})


if __name__ == '__main__':
    unittest.main()
