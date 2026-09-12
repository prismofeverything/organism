"""One-time export of trusted local Python trainer weights for native warm start.

Usage: .venv-training/bin/python native/import_python_weights.py SOURCE DESTINATION
Optimizer/replay remain in SOURCE; this exports only network weights.
"""
import json
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import torch
source, destination = map(Path, sys.argv[1:3])
destination.mkdir(parents=True, exist_ok=True)
for players in (2, 3):
    path = source / f'{players}p/latest.pt'
    if not path.exists():
        continue
    checkpoint = torch.load(path, map_location='cpu', weights_only=False)
    out = destination / f'{players}p.pt'
    torch.save(dict(checkpoint['network']), out)
    out.with_suffix('.json').write_text(json.dumps({'source': str(path.resolve()), 'iteration': checkpoint['iteration'], 'contents': 'network weights only'}))
    print(f'{players}p: imported weights from Python iteration {checkpoint["iteration"]} -> {out}')
