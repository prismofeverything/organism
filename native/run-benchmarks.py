"""Resumable fixed-opponent strength measurements, independent of training.

Run with --watch to capture another candidate after >=100 new iterations.
STOP in the benchmark root gracefully stops its child, without stopping training.
"""
import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time


def read(path, default=None):
    try:
        return json.loads(path.read_text())
    except (FileNotFoundError, json.JSONDecodeError):
        return default


def write(path, value):
    temp = path.with_suffix('.tmp')
    temp.write_text(json.dumps(value, indent=2))
    temp.replace(path)


def freeze(source, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, target)
    return hashlib.sha256(target.read_bytes()).hexdigest()


def initialize(root, training, binary):
    if (root / 'protocol.json').exists():
        return read(root / 'protocol.json')
    anchors = {
        '2p-r3': [('initial-379', Path('checkpoints/organism-ablation-20260912/initial/model.ot')),
                  ('control-399', Path('checkpoints/organism-ablation-20260912/control/evaluation-020/candidate.ot'))],
        '3p': [('older-385', training / '3p/opponent-archive/000000385.ot'),
               ('older-625', training / '3p/opponent-archive/000000625.ot')],
    }
    spec = {'version': 1, 'simulations': 64, 'games_per_seat': 16, 'shards': 4,
            'seed': 730013, 'cutoff_value': 'draw', 'interval': 100, 'opponents': {}}
    for model, sources in anchors.items():
        spec['opponents'][model] = []
        for identity, source in sources:
            path = root / 'opponents' / model / (identity + '.ot')
            sha = freeze(source, path)
            spec['opponents'][model].append({'identity': identity, 'weights': str(path), 'sha256': sha})
    spec['binary_sha256'] = freeze(binary, root / 'organism-train')
    write(root / 'protocol.json', spec)
    return spec


def capture(root, training, model, spec):
    previous = sorted((root / 'candidates' / model).glob('*/candidate.json'))
    latest = read(training / model / 'latest.json')
    if not latest:
        return
    generation = latest['generation']
    iteration = int(generation.split('-')[0])
    if previous and iteration < max(read(p)['iteration'] for p in previous) + spec['interval']:
        return
    config = read(training / model / 'config.json')
    if (config['players'], config['rings']) != ((2, 3) if model == '2p-r3' else (3, 4)):
        raise RuntimeError('Board changed; start a new benchmark protocol for the new board')
    base = root / 'candidates' / model / f'{iteration:06d}'
    base.mkdir(parents=True, exist_ok=True)
    try:
        sha = freeze(training / model / 'snapshots' / generation / 'model.ot', base / 'candidate.ot')
    except FileNotFoundError:
        return  # Trainer pruned the generation; retry its next completed snapshot.
    candidate = {'identity': f'{model}-iteration-{iteration}', 'iteration': iteration,
                 'weights': str(base / 'candidate.ot'), 'sha256': sha}
    # Interleave opponents within each shard; seats remain balanced in each batch.
    for shard in range(spec['shards']):
        for opponent in spec['opponents'][model]:
            job = base / f"batch-{shard}-{opponent['identity']}"
            job.mkdir(exist_ok=True)
            manifest = {'config': config, 'candidate': candidate, 'opponent': opponent,
                        'simulations': spec['simulations'], 'games_per_seat': spec['games_per_seat']//spec['shards'],
                        'seed': spec['seed'] + shard * 1000, 'cutoff_value': spec['cutoff_value']}
            write(job / 'manifest.json', manifest)
    write(base / 'candidate.json', candidate)


def summarize(root, training, spec):
    result = {'updated': time.time(), 'protocol': spec, 'models': {}}
    pending = []
    for model in spec['opponents']:
        rows = []
        for path in sorted((root / 'candidates' / model).glob('*/candidate.json')):
            candidate = read(path)
            opponents = []
            for opponent in spec['opponents'][model]:
                total = (2 if model == '2p-r3' else 3) * spec['games_per_seat']
                counts = dict(wins=0, losses=0, cutoffs=0, completed=0, choices=0)
                for manifest in sorted(path.parent.glob(f"batch-*-{opponent['identity']}/manifest.json")):
                    report = read(manifest.parent / 'report.json', {})
                    for key in ['wins', 'losses', 'cutoffs']:
                        counts[key] += report.get(key, 0)
                    counts['completed'] += report.get('completed_games', 0)
                    counts['choices'] += report.get('total_choices', 0)
                    if not report.get('complete'):
                        pending.append(manifest)
                opponents.append({'identity': opponent['identity'], 'total': total, **counts})
            rows.append({'iteration': candidate['iteration'], 'opponents': opponents})
        result['models'][model] = rows
    write(training / 'benchmarks.json', result)
    # Round-robin models, then shards/opponents, to avoid starving either model.
    queues = [sorted([p for p in pending if p.parents[2].name == model], key=lambda p: (p.parents[1].name, p.parent.name)) for model in spec['opponents']]
    ordered = [q[i] for i in range(max(map(len, queues), default=0)) for q in queues if i < len(q)]
    return ordered


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path('checkpoints/organism-benchmark-20260913'))
    parser.add_argument('--training', type=Path, default=Path('checkpoints/organism-native'))
    parser.add_argument('--watch', action='store_true')
    args = parser.parse_args()
    root, training = args.root.absolute(), args.training.absolute()
    root.mkdir(parents=True, exist_ok=True)
    lock = (root / 'runner.lock').open('w')
    fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    os.nice(10)
    spec = initialize(root, training, Path('native/target/release/organism-train').resolve())
    if hashlib.sha256((root / 'organism-train').read_bytes()).hexdigest() != spec['binary_sha256']:
        raise RuntimeError('Frozen benchmark executable changed')
    while not (root / 'STOP').exists():
        for model in spec['opponents']:
            existing = list((root / 'candidates' / model).glob('*/batch-*/manifest.json'))
            if all(read(p.parent / 'report.json', {}).get('complete') for p in existing):
                capture(root, training, model, spec)
        jobs = summarize(root, training, spec)
        if not jobs:
            write(root / 'status.json', {'stage': 'waiting' if args.watch else 'complete', 'updated': time.time()})
            if not args.watch:
                break
            time.sleep(30)
            continue
        for manifest in jobs:
            if (root / 'STOP').exists():
                break
            spec_job = read(manifest)
            for key in ['candidate', 'opponent']:
                item = spec_job[key]
                if hashlib.sha256(Path(item['weights']).read_bytes()).hexdigest() != item['sha256']:
                    raise RuntimeError('Frozen benchmark weights changed')
            stop = manifest.parent / 'STOP'
            stop.unlink(missing_ok=True)
            write(root / 'status.json', {'stage': 'evaluation', 'manifest': str(manifest), 'updated': time.time()})
            with (manifest.parent / 'run.log').open('a') as log:
                process = subprocess.Popen([str(root / 'organism-train'), 'compare', str(manifest)], stdout=log, stderr=subprocess.STDOUT)
                while process.poll() is None:
                    if (root / 'STOP').exists():
                        stop.touch()
                    summarize(root, training, spec)
                    time.sleep(3)
                if process.returncode:
                    raise RuntimeError(f'Benchmark failed; see {manifest.parent / "run.log"}')
            summarize(root, training, spec)
    write(root / 'status.json', {'stage': 'stopped' if (root / 'STOP').exists() else 'complete', 'updated': time.time()})


if __name__ == '__main__':
    main()
