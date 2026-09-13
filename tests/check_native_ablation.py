"""End-to-end three-arm budgets, frozen inputs, evaluation and restart checks."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

repo = Path(__file__).resolve().parents[1]
root = Path(tempfile.mkdtemp(prefix='organism-ablation-test-'))
env = dict(os.environ)
env['LD_LIBRARY_PATH'] = str(repo/'.venv-training/lib/python3.12/site-packages/torch/lib')
exe = repo/'native/target/release/organism-train'
with (root/'log').open('w') as log:
    subprocess.run([str(exe), 'train', '--cpu', '--players', '2', '--rings-2p', '3',
                    '--actors', '2', '--concurrent-games', '2', '--sims', '2', '--max-steps', '20',
                    '--blocks', '1', '--filters', '8', '--train-steps', '1', '--buffer', '32',
                    '--eval-every', '0', '--checkpoint', str(root/'seed')],
                   env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)
    model = root/'seed/2p-r3'
    generation = json.loads((model/'latest.json').read_text())['generation']
    source = root/'source';shutil.copytree(model/'snapshots'/generation, source)
    shutil.copy2(model/'config.json', source/'config.json')
    experiment = root/'experiment'
    command = [sys.executable, str(repo/'native/run-ablation.py'), '--cpu', '--source', str(source),
               '--root', str(experiment), '--rounds', '2', '--milestones', '1,2',
               '--evaluation-games-per-seat', '1', '--duty', '1']
    subprocess.run(command+['--prepare-only'], env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)
    spec = json.loads((experiment/'experiment.json').read_text())
    for name in spec['arms']:
        for filename in ['state.json', 'model.ot', 'adam.ot']:
            path = experiment/name/'2p-r3/snapshots/initial'/filename
            assert hashlib.sha256(path.read_bytes()).hexdigest() == spec['initial_hashes'][filename]
    (experiment/'STOP').touch()
    subprocess.run(command, env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)
    assert json.loads((experiment/'status.json').read_text())['stage'] == 'stopped'
    (experiment/'STOP').unlink()
    subprocess.run(command, env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=90)
    assert json.loads((experiment/'status.json').read_text())['stage'] == 'complete'
    endpoints = {}
    for name, settings in spec['arms'].items():
        model = experiment/name/'2p-r3'
        gen = json.loads((model/'latest.json').read_text())['generation']
        state = json.loads((model/'snapshots'/gen/'state.json').read_text())
        assert state['iteration'] == spec['initial_iteration']+2
        assert state['last_metrics']['optimizer_step'] == 3
        assert state['search_settings']['mask_cutoffs'] == (settings['cutoff']=='mask')
        assert state['search_settings']['replay_game_cap'] == settings['cap']
        assert state['config']['replay'] == settings['buffer']
        for milestone in [1,2]:
            report = json.loads((experiment/name/f'evaluation-{milestone:03}/report.json').read_text())
            assert report['complete'] and len(report['games']) == 2
            assert report['manifest']['opponent']['sha256'] == spec['initial_hashes']['model.ot']
            assert report['manifest']['cutoff_value'] == 'draw'
        endpoints[name] = gen
    subprocess.run(command, env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)
    for name, gen in endpoints.items():
        assert json.loads((experiment/name/'2p-r3/latest.json').read_text())['generation'] == gen
print('Three-arm identical inputs, equal updates, frozen evaluations and idempotent resume passed:', root)
