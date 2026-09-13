#!/usr/bin/env python3
"""Run isolated replay/cutoff ablations; all game search and training stay native."""
import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

REPO = Path(__file__).resolve().parents[1]
ARMS = {
    'control': {'buffer': 5000, 'cap': 0, 'cutoff': 'draw'},
    'replay_only': {'buffer': 32768, 'cap': 256, 'cutoff': 'draw'},
    'cutoff_only': {'buffer': 5000, 'cap': 0, 'cutoff': 'mask'},
}

def atomic(path, value):
    tmp = path.with_suffix('.tmp.json')
    tmp.write_text(json.dumps(value, indent=2))
    tmp.replace(path)

def digest(path):
    h = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()

def saved_integer(path, key):
    # Native Saved is large. Read a scalar without materializing replay as Python objects.
    pattern = re.compile(rb'"' + key.encode() + rb'"\s*:\s*(\d+)')
    with path.open('rb') as stream:
        previous = b''
        for block in iter(lambda: stream.read(65536), b''):
            data = previous + block
            match = pattern.search(data)
            if match:
                return int(match[1])
            previous = data[-128:]
    raise ValueError(f'Missing {key} in {path}')

def snapshot(model):
    name = json.loads((model / 'latest.json').read_text())['generation']
    if Path(name).name != name or name in ('.', '..'):
        raise ValueError('Invalid generation')
    return model / 'snapshots' / name

def prepare(root, source, rounds, milestones, evaluation_games, duty, cpu):
    spec_path = root / 'experiment.json'
    if spec_path.exists():
        spec = json.loads(spec_path.read_text())
        if (spec['source'] != str(source) or spec['rounds'] != rounds or
            spec['milestones'] != milestones or spec['evaluation_games_per_seat'] != evaluation_games or
            spec['cpu'] != cpu or spec['duty'] != duty):
            raise ValueError('Experiment settings differ; resume with original arguments')
        return spec
    config = json.loads((source / 'config.json').read_text())
    if config['players'] != 2 or config['rings'] != 3:
        raise ValueError('This experiment is for two-player, three-ring checkpoints')
    if saved_integer(source / 'state.json', 'training_step') != 0:
        raise ValueError('Start from a checkpoint before gradient updates for its next iteration')
    initial = saved_integer(source / 'state.json', 'iteration')
    seed = root / 'initial'
    seed.mkdir(exist_ok=True)
    hashes = {}
    for name in ['state.json', 'model.ot', 'adam.ot', 'config.json']:
        target = seed / name
        if not target.exists():
            shutil.copy2(source / name, target)
        hashes[name] = digest(target)
        if hashes[name] != digest(source / name):
            raise ValueError(f'Initial fixture differs: {name}')
    executable = seed / 'organism-train'
    if not executable.exists():
        shutil.copy2(REPO / 'native/target/release/organism-train', executable)
    hashes['organism-train'] = digest(executable)
    arms = {name: dict(options) for name, options in ARMS.items()}
    # The control inherits the source replay capacity, including tiny CPU test fixtures.
    arms['control']['buffer'] = config['replay']
    arms['cutoff_only']['buffer'] = config['replay']
    for name in arms:
        model = root / name / '2p-r3'
        generation = model / 'snapshots' / 'initial'
        generation.mkdir(parents=True, exist_ok=True)
        for filename in ['state.json', 'model.ot', 'adam.ot']:
            if not (generation / filename).exists():
                os.link(seed / filename, generation / filename)
        shutil.copy2(seed / 'config.json', model / 'config.json')
        # No inherited pending evaluations, curriculum or production state.
        shutil.copy2(seed / 'model.ot', model / 'baseline.ot')
        atomic(model / 'latest.json', {'generation': 'initial'})
    spec = {'version': 1, 'source': str(source), 'initial_iteration': initial,
            'rounds': rounds, 'milestones': milestones, 'config': config,
            'initial_hashes': hashes, 'arms': arms, 'cpu': cpu, 'duty': duty,
            'evaluation_games_per_seat': evaluation_games,
            'evaluation_cutoff': 'draw', 'evaluation_seed': 9917,
            'search_protocol': 3, 'initial_rng': 'identical persisted checkpoint RNG in every arm',
            'notes': 'One training trajectory per arm; screen, not a statistically conclusive learning study.'}
    atomic(spec_path, spec)
    return spec

def run(root, spec):
    env = dict(os.environ)
    torch = REPO / '.venv-training/lib/python3.12/site-packages/torch/lib'
    env['LD_LIBRARY_PATH'] = str(torch) + ':' + env.get('LD_LIBRARY_PATH', '')
    exe = root / 'initial/organism-train'
    config = spec['config']
    initial = spec['initial_iteration']
    active_stop = None

    def launch(command, log, stop):
        nonlocal active_stop
        active_stop = stop
        if stop.exists():
            stop.unlink()  # Only this experiment's earlier forwarded stop marker.
        with log.open('a') as output:
            process = subprocess.Popen(command, cwd=REPO, env=env, stdout=output, stderr=subprocess.STDOUT)
            try:
                while process.poll() is None:
                    if (root / 'STOP').exists():
                        stop.touch()
                    time.sleep(0.5)
                if process.returncode:
                    raise RuntimeError(f'Native process failed ({process.returncode}); see {log}')
            except BaseException:
                stop.touch()
                process.wait(timeout=180)
                raise
        active_stop = None

    try:
        # Rotate branch order each round to spread system-load/time-order effects.
        names = list(spec['arms'])
        for round_number in range(1, spec['rounds'] + 1):
            order = names[(round_number-1) % len(names):] + names[:(round_number-1) % len(names)]
            for name in order:
                if (root / 'STOP').exists():
                    atomic(root / 'status.json', {'stage': 'stopped', 'updated': time.time()})
                    return
                arm = root / name
                model = arm / '2p-r3'
                target = initial + round_number
                current = saved_integer(snapshot(model) / 'state.json', 'iteration')
                if current < target:
                    settings = spec['arms'][name]
                    command = [str(exe), 'train', '--players', '2', '--rings-2p', '3', '--checkpoint', str(arm),
                               '--iters', '1', '--eval-every', '0', '--concurrent-games', str(config['actors'] if spec['cpu'] else 64), '--gpu-batch', '32',
                               '--exploration-rounds', '10', '--threads', '2', '--duty', str(spec['duty']),
                               '--vram-fraction', '0.20', '--buffer', str(settings['buffer']),
                               '--replay-game-cap', str(settings['cap']), '--cutoff-value', settings['cutoff']]
                    for flag, key in [('blocks','blocks'), ('filters','filters'), ('sims','sims'),
                                      ('actors','actors'), ('max-steps','max_steps'), ('repetition','repetition'),
                                      ('batch-size','batch'), ('train-steps','train_steps')]:
                        command += ['--' + flag, str(config[key])]
                    if spec['cpu']:
                        command += ['--cpu']
                    atomic(root / 'status.json', {'stage': 'training', 'arm': name, 'round': round_number,
                           'target_iteration': target, 'updated': time.time(), 'command': command})
                    launch(command, arm / 'training.log', arm / 'STOP')
                    if (root / 'STOP').exists():
                        atomic(root / 'status.json', {'stage': 'stopped', 'updated': time.time()})
                        return
                    actual = saved_integer(snapshot(model) / 'state.json', 'iteration')
                    if actual != target:
                        raise RuntimeError(f'{name}: expected iteration {target}, got {actual}')
                if round_number in spec['milestones']:
                    evaluation = arm / f'evaluation-{round_number:03}'
                    evaluation.mkdir(exist_ok=True)
                    manifest = evaluation / 'manifest.json'
                    if not manifest.exists():
                        endpoint = snapshot(model)
                        if saved_integer(endpoint / 'state.json', 'iteration') != target:
                            raise RuntimeError('Missing historical evaluation endpoint; refusing to substitute newer weights')
                        shutil.copy2(endpoint / 'model.ot', evaluation / 'candidate.ot')
                        atomic(manifest, {'config': config, 'candidate': {'weights': 'candidate.ot',
                               'identity': f'{name}-iteration-{target}', 'sha256': digest(evaluation/'candidate.ot')},
                               'opponent': {'weights': str(root/'initial/model.ot'), 'identity': f'initial-iteration-{initial}',
                               'sha256': spec['initial_hashes']['model.ot']}, 'simulations': config['sims'],
                               'games_per_seat': spec['evaluation_games_per_seat'], 'seed': spec['evaluation_seed'],
                               'cutoff_value': spec['evaluation_cutoff']})
                    report = evaluation / 'report.json'
                    if not report.exists() or not json.loads(report.read_text())['complete']:
                        atomic(root / 'status.json', {'stage': 'evaluation', 'arm': name, 'round': round_number,
                               'updated': time.time(), 'report': str(report)})
                        command = ['nice', '-n', '10', str(exe), 'compare', str(manifest)]
                        if spec['cpu']:
                            command.append('--cpu')
                        launch(command, evaluation / 'evaluation.log', evaluation / 'STOP')
                        if (root / 'STOP').exists():
                            atomic(root / 'status.json', {'stage': 'stopped', 'updated': time.time()})
                            return
            atomic(root / 'progress.json', {'completed_rounds': round_number,
                   'updates_per_arm': round_number * config['train_steps'], 'updated': time.time()})
        atomic(root / 'status.json', {'stage': 'complete', 'updated': time.time()})
    except BaseException as error:
        atomic(root / 'status.json', {'stage': 'error', 'message': str(error), 'updated': time.time()})
        raise

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, default=REPO/'checkpoints/organism-comparison-20260912/old')
    parser.add_argument('--root', type=Path, default=REPO/'checkpoints/organism-ablation-20260912')
    parser.add_argument('--rounds', type=int, default=20)
    parser.add_argument('--milestones', default='5,20')
    parser.add_argument('--evaluation-games-per-seat', type=int, default=8)
    parser.add_argument('--duty', type=float, default=0.5)
    parser.add_argument('--cpu', action='store_true')
    parser.add_argument('--prepare-only', action='store_true')
    args = parser.parse_args()
    milestones = sorted(set(map(int, args.milestones.split(','))))
    if args.rounds < 1 or not milestones or min(milestones) < 1 or max(milestones) > args.rounds or args.evaluation_games_per_seat < 1 or not 0 < args.duty <= 1:
        parser.error('Invalid round, milestone, evaluation or duty settings')
    root = args.root.absolute(); root.mkdir(parents=True, exist_ok=True)
    with (root / 'orchestrator.lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        spec = prepare(root, args.source.absolute(), args.rounds, milestones,
                       args.evaluation_games_per_seat, args.duty, args.cpu)
        if not args.prepare_only:
            run(root, spec)
    print(root, flush=True)

if __name__ == '__main__':
    main()
