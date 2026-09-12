import tempfile
import unittest
from pathlib import Path

import numpy as np
import torch

from alphazero.games.organism.game import OrganismGame
from alphazero.mcts import MCTS, Node
from alphazero.network import AlphaZeroNetwork
from alphazero.self_play import self_play_game, _partial_rewards
from alphazero.train import Trainer
from alphazero.train_organism import BackgroundControl, StopTraining


class TrainingTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        torch.set_num_threads(1)

    def test_board_and_encoding(self):
        for n in (2, 3, 5):
            game = OrganismGame(n)
            state = game.initial_state()
            encoding = game.encode_state(state, game.current_player(state))
            self.assertEqual(encoding.shape[1:], (36, 36) if n < 5 else (30, 30))
            starts = [s for _, info in game._player_info for s in info['starting_spaces']]
            self.assertEqual(len(set(starts)), 3 * n)
            self.assertTrue(all(s in game.adjacencies for s in starts))
            for _ in range(12):
                legal = game.legal_actions(state)
                self.assertTrue(legal)
                self.assertTrue(all(0 <= a < game.action_space_size() for a in legal))
                state = next(iter(legal.values()))
                self.assertEqual(game.encode_state(state, game.current_player(state)).shape, encoding.shape)

    def test_branch_isolation_and_equal_rewards(self):
        game = OrganismGame(2)
        state = game.initial_state()
        branches = list(game.legal_actions(state).values())
        branches[0]["state"]["food"][(0, 0)] = 99
        self.assertNotIn((0, 0), branches[1]["state"]["food"])
        self.assertNotIn((0, 0), state["state"]["food"])
        self.assertEqual(set(_partial_rewards(state).values()), {0.0})

    def test_terminal_automatic_resolution(self):
        game = OrganismGame(2)
        state = game.initial_state()
        player = game.current_player(state)
        state['state']['captures'][player] = [{}] * 5
        resolved = game._advance_automatic(state)
        self.assertTrue(game.is_terminal(resolved))
        self.assertEqual(game.rewards(resolved)[player], 1)
        self.assertFalse(game.is_terminal(state))

    def test_multiplayer_values_and_legal_policy(self):
        game = OrganismGame(3)
        net = AlphaZeroNetwork.for_game(game, num_res_blocks=1, num_filters=4).eval()
        with torch.no_grad():
            net.value_fc2.weight.zero_()
            net.value_fc2.bias.copy_(torch.tensor([0.1, 0.5, -0.7]))
        state = game.initial_state()
        state['state']['player_turn']['player'] = state['turn_order'][1]
        search = MCTS(game, net, num_simulations=2)
        values = search._evaluate(Node(state, game.current_player(state), 1))
        self.assertAlmostEqual(values[state['turn_order'][2]], np.tanh(0.5), places=5)
        self.assertAlmostEqual(values[state['turn_order'][0]], np.tanh(-0.7), places=5)
        calls = []
        handle = net.register_forward_hook(lambda *_: calls.append(1))
        policy = search.policy(state)
        handle.remove()
        self.assertLessEqual(len(calls), 3)  # root plus two simulations, once each
        self.assertAlmostEqual(float(policy.sum()), 1)
        self.assertTrue(set(np.flatnonzero(policy)) <= set(game.legal_actions(state)))

    def test_resume_optimizer_replay_and_rng(self):
        with tempfile.TemporaryDirectory() as directory:
            kwargs = dict(checkpoint_dir=directory, num_res_blocks=1, num_filters=4,
                          games_per_iteration=1, mcts_simulations=1, max_steps_per_game=2,
                          batch_size=2, min_buffer_size=1, train_steps_per_iter=1,
                          replay_buffer_capacity=8, truncation='draw')
            trainer = Trainer(OrganismGame(2), **kwargs)
            trainer.run(1)
            params = {k: v.clone() for k, v in trainer.network.state_dict().items()}
            expected = np.random.random()
            resumed = Trainer(OrganismGame(2), **kwargs)
            resumed.resume()
            self.assertEqual(resumed.iteration, 1)
            self.assertEqual(len(resumed.replay_buffer), 2)
            self.assertTrue(resumed.optimizer.state)
            self.assertEqual(np.random.random(), expected)
            for key, value in resumed.network.state_dict().items():
                self.assertTrue(torch.equal(params[key], value))
            resumed.run(1)
            self.assertEqual(resumed.iteration, 2)

    def test_repetition_ends_self_play_without_inventing_a_winner(self):
        class LoopGame:
            def initial_state(self, players=None):
                return {"turn_order": ["a", "b"], "state": {"winner": None}}
            def current_player(self, state):
                return "a"
            def is_terminal(self, state):
                return False
            def legal_actions(self, state):
                return {0: state}
            def action_space_size(self):
                return 1
            def encode_state(self, state, player):
                return np.zeros((1, 2, 2), dtype=np.float32)
            def action_to_index(self, action):
                return action
            def index_to_action(self, index):
                return index
            def repetition_key(self, state):
                return 0
        net = AlphaZeroNetwork(1, 2, 1, num_res_blocks=1, num_filters=4).eval()
        stats = {}
        samples = self_play_game(LoopGame(), net, num_simulations=1, max_steps=10,
                                 repetition_limit=3, truncation='draw', stats=stats)
        self.assertEqual(len(samples), 2)
        self.assertEqual(stats['termination'], 'repetition')
        self.assertFalse(stats['terminal'])
        self.assertTrue(all(s['value'] == 0 for s in samples))

    def test_discard_and_stop(self):
        game = OrganismGame(2)
        net = AlphaZeroNetwork.for_game(game, num_res_blocks=1, num_filters=4).eval()
        stats = {}
        self.assertEqual(self_play_game(game, net, num_simulations=1, max_steps=1,
                                       truncation='discard', stats=stats), [])
        self.assertFalse(stats['terminal'])
        with tempfile.TemporaryDirectory() as directory:
            stop = Path(directory) / 'STOP'
            control = BackgroundControl(stop, 1, 'cpu')
            control()
            stop.touch()
            with self.assertRaises(StopTraining):
                control()


if __name__ == '__main__':
    unittest.main()
