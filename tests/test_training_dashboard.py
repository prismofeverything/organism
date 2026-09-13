import json
from pathlib import Path
import tempfile
import unittest

from alphazero.games.organism.game import OrganismGame
from alphazero.telemetry import GameRecorder
from alphazero.dashboard import metrics
from pieces.organism_format import load_ogf, board_locations


class RecordingTests(unittest.TestCase):
    def test_recording_compatible_with_existing_ogf_reader(self):
        with tempfile.TemporaryDirectory() as directory:
            game = OrganismGame(num_players=3, num_rings=4, remove_notches=False)
            recorder = GameRecorder(directory, game, 2, 1)
            state = game.initial_state()
            palette = recorder.data['board']['ring-colors'][:]
            other = GameRecorder(directory, game, 2, 2)
            self.assertNotEqual(palette, other.data['board']['ring-colors'])
            recorder.observe(state, 0)
            for step in range(1, 30):
                legal = game.legal_actions(state)
                if not legal:
                    break
                action = next(iter(legal))
                state = legal[action]
                recorder.observe(state, step, action)
            recorder.finish({'terminal': False, 'termination': 'max_steps', 'steps': step})
            path = next((Path(directory) / 'games').glob('*.json'))
            recorded = load_ogf(path)
            self.assertEqual(recorded['format'], 'organism')
            self.assertEqual(recorded['version'], 2)
            self.assertEqual(recorded['board']['ring-colors'], palette)
            self.assertEqual(recorded['board']['center'], 'A0')
            self.assertNotIn('colors', recorded)
            self.assertTrue(all(c.startswith('hsl(') for c in recorded['board']['ring-colors']))
            self.assertEqual(len(recorded['frames']), step + 1)
            self.assertEqual(set(board_locations(recorded)), set(recorded['board']['spaces']))
            frame = recorded['frames'][-1]
            self.assertEqual(len(frame['elements']), len(state['state']['elements']))
            self.assertTrue(all(len(e) == 4 for e in frame['elements']))
            self.assertEqual(recorded['frames'][0]['elements'], [])
            self.assertEqual(json.loads((Path(directory) / 'live.json').read_text()), recorded)

    def test_metrics_tolerates_incomplete_append_and_bounds_history(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'metrics.jsonl'
            path.write_text(''.join(json.dumps({'iteration': i})+'\n' for i in range(510))+'{"iteration":')
            rows = metrics(path)
            self.assertEqual(rows[-1]['iteration'], 509)
            self.assertEqual(len(rows), 499)
