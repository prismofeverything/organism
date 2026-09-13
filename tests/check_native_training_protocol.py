"""Controlled cutoff ablation, replay cap/migration, and mixed frozen opponents."""
import json, subprocess, tempfile
from pathlib import Path

root = Path(tempfile.mkdtemp(prefix='organism-protocol-'))
exe = Path('native/target/release/organism-train').resolve()
flags = ['train', '--cpu', '--players', '3', '--actors', '2', '--concurrent-games', '2',
         '--gpu-batch', '2', '--sims', '4', '--max-steps', '20', '--blocks', '1',
         '--filters', '8', '--train-steps', '1', '--batch-size', '8', '--replay-game-cap', '8',
         '--eval-every', '1', '--eval-games-per-seat', '2']

def run(mode, iterations, capacity):
    with (root / f'{mode}.log').open('a') as log:
        subprocess.run([str(exe), *flags, '--checkpoint', str(root / mode),
                        '--cutoff-value', mode, '--iters', str(iterations), '--buffer', str(capacity)],
                       stdout=log, stderr=subprocess.STDOUT, check=True, timeout=90)
    model = root / mode / '3p'
    generation = json.loads((model / 'latest.json').read_text())['generation']
    return json.loads((model / 'snapshots' / generation / 'state.json').read_text())

mask = run('mask', 1, 32)
draw = run('draw', 1, 32)
for name, state, weight in [('mask', mask, 0), ('draw', draw, 1)]:
    assert len(state['replay']) == 16
    assert len({s['game_id'] for s in state['replay']}) == 2
    assert all(s['termination'] == 'max_steps' and s['value_weight'] == weight for s in state['replay'])
    assert state['last_metrics']['replay_value_supervised_fraction'] == weight
    if not weight:
        assert state['last_metrics']['value_loss'] == 0
# Same initial game seeds/weights; this tiny experiment validates mechanics, not playing strength.
assert [s['game_id'] for s in mask['replay']] == [s['game_id'] for s in draw['replay']]
expanded = run('mask', 1, 64)
assert expanded['config']['replay'] == 64 and len(expanded['replay']) == 32
assert expanded['replay'][:16] == mask['replay']
report = json.loads((root / 'mask/3p/evaluation.json').read_text())
assert report['protocol'] == 'mixed-history' and len(report['opponent_ids']) == 2
assert len(report['games']) == 6
for seat in range(3):
    games = [g for g in report['games'] if g['seat'] == seat]
    assert len(games) == 2
    assert all(sorted(g['seat_models']) == [0, 1, 2] for g in games)
    assert games[0]['seat_models'] != games[1]['seat_models']
print('Cutoff mask/draw, per-game replay cap, capacity migration and mixed evaluation passed:', root)
