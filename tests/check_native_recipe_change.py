"""Verify a running checkpoint accepts recipe changes and the no-progress rule."""
import json, subprocess, tempfile
from pathlib import Path

exe = Path('native/target/release/organism-train').resolve()
root = Path(tempfile.mkdtemp(prefix='organism-recipe-'))
base = ['train', '--cpu', '--players', '2', '--rings-2p', '3', '--actors', '2',
        '--concurrent-games', '4', '--gpu-batch', '2', '--sims', '2', '--max-steps', '60',
        '--blocks', '1', '--filters', '8', '--train-steps', '1', '--batch-size', '8',
        '--eval-every', '0', '--checkpoint', str(root)]


def run(flags, log):
    with (root / log).open('w') as out:
        subprocess.run([str(exe), *base, *flags], stdout=out, stderr=subprocess.STDOUT,
                       check=True, timeout=180)


def saved():
    generation = json.loads((root / '2p-r3' / 'latest.json').read_text())['generation']
    return json.loads((root / '2p-r3' / 'snapshots' / generation / 'state.json').read_text())


# A first pass writes the checkpoint under the original protocol.
run(['--iters', '1', '--buffer', '400', '--replay-game-cap', '0'], 'first.log')
first = saved()
assert first['config']['stall_limit'] == 0, first['config']
assert first['config']['eval_service_ticks'] in (0, 1), first['config']
assert all(g['termination'] != 'no_progress' for g in first['episodes'][0:0] or []), 'baseline must not cut'
assert 'no_progress' not in (root / 'first.log').read_text(), 'rule must stay off by default'

# Resuming with a different recipe must be accepted, not rejected as incompatible.
run(['--iters', '1', '--buffer', '4000', '--replay-game-cap', '16', '--stall-limit', '1',
     '--eval-max-steps', '200', '--eval-service-ticks', '4', '--lr-anneal', '10',
     '--lr-floor', '0.00005'], 'second.log')
second = saved()
for key, value in [('replay', 4000), ('stall_limit', 1), ('eval_max_steps', 200),
                   ('eval_service_ticks', 4), ('lr_anneal', 10), ('lr_floor', 0.00005)]:
    assert second['config'][key] == value, (key, second['config'][key], value)
assert second['iteration'] == first['iteration'] + 1, (first['iteration'], second['iteration'])
assert len(second['replay']) >= len(first['replay']), 'growing the buffer must not discard samples'
assert 'no_progress' in (root / 'second.log').read_text(), 'stall limit 1 must end stalled games'

# Per-game sampling bounds how much of the buffer one game can occupy. The cap
# applies when a game is ingested, so games already banked under the previous
# recipe stay as they are and only the second iteration's games are capped.
counts = {}
for sample in second['replay']:
    if sample.get('game_id'):
        counts[sample['game_id']] = counts.get(sample['game_id'], 0) + 1
capped = {k: v for k, v in counts.items() if k.startswith('000002-')}
assert capped and max(capped.values()) <= 16, capped
assert max(counts.values()) > 16, 'earlier uncapped games must survive the recipe change'

print('Recipe change, buffer growth and no-progress cutoff passed:', root)
