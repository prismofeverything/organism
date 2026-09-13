"""Verify a 2p recipe override leaves the concurrent 3p recipe unchanged."""
import json, subprocess, tempfile
from pathlib import Path
root = Path(tempfile.mkdtemp(prefix='organism-player-settings-'))
flags = ['train', '--cpu', '--players', '2,3', '--iters', '1', '--actors', '2',
         '--concurrent-games', '2', '--gpu-batch', '2', '--sims', '2', '--max-steps', '20',
         '--blocks', '1', '--filters', '8', '--train-steps', '1', '--batch-size', '8',
         '--eval-every', '0', '--buffer', '64', '--replay-game-cap', '4', '--cutoff-value', 'mask',
         '--buffer-2p', '32', '--replay-game-cap-2p', '0', '--cutoff-value-2p', 'draw',
         '--checkpoint', str(root)]
with (root / 'run.log').open('w') as log:
    subprocess.run([str(Path('native/target/release/organism-train').resolve()), *flags],
                   stdout=log, stderr=subprocess.STDOUT, check=True, timeout=90)
for players, capacity, cap, mask in [(2, 32, 0, False), (3, 64, 4, True)]:
    base = root / f'{players}p'
    generation = json.loads((base / 'latest.json').read_text())['generation']
    state = json.loads((base / 'snapshots' / generation / 'state.json').read_text())
    assert state['config']['replay'] == capacity
    assert state['search_settings']['replay_game_cap'] == cap
    assert state['search_settings']['mask_cutoffs'] == mask
    assert state['replay'] and all(s['value_weight'] == (0 if mask else 1) for s in state['replay'])
print('Per-player settings and cutoff targets passed:', root)
