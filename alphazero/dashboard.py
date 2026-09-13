"""Local, read-only training dashboard. python -m alphazero.dashboard"""
import argparse
from collections import deque
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
from pathlib import Path
import re
import time
from urllib.parse import urlsplit


def read_json(path, default=None):
    try:
        return json.loads(path.read_text())
    except (OSError, ValueError):
        return default


def metrics(path):
    try:
        with path.open() as stream:
            lines = deque(stream, maxlen=500)
        result = []
        for line in lines:
            try:
                result.append(json.loads(line))
            except ValueError:
                pass  # writer may still be appending the last line
        return result
    except OSError:
        return []


def make_handler(root, examples):
    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_GET(self):
            path = urlsplit(self.path).path
            if path == '/':
                return self.send_bytes(Path(__file__).with_name('dashboard.html').read_bytes(), 'text/html; charset=utf-8')
            if path == '/viewer/main.js':
                asset = Path(__file__).resolve().parents[1] / 'resources/public/js/ogf-viewer/main.js'
                try:
                    return self.send_bytes(asset.read_bytes(), 'text/javascript; charset=utf-8')
                except OSError:
                    return self.send_error(503, 'Build the shared renderer: npx shadow-cljs release ogf-viewer')
            if path == '/api/benchmarks':
                return self.send_json(read_json(root / 'benchmarks.json', {'models': {}}))
            if path == '/api/status':
                models = []
                for directory in sorted(root.glob('[2-5]p*')):
                    status = read_json(directory / 'status.json', {})
                    history = metrics(directory / 'metrics.jsonl')
                    records = []
                    for file in sorted((directory / 'games').glob('*.json'), reverse=True):
                        # Metadata cache avoids reparsing full games on every poll.
                        try:
                            stamp = file.stat().st_mtime_ns
                        except FileNotFoundError:
                            continue  # Trainer may prune a recording after glob().
                        cached = self.server.record_cache.get(str(file))
                        if not cached or cached[0] != stamp:
                            data = read_json(file, {})
                            meta = {k: data.get(k) for k in ('name', 'iteration', 'number', 'result', 'finished')}
                            meta['url'] = f'/api/game/{directory.name}/{file.name}'
                            cached = (stamp, meta)
                            self.server.record_cache[str(file)] = cached
                        records.append(cached[1])
                    models.append({'model': directory.name, 'config': read_json(directory / 'config.json', {}), 'status': status, 'metrics': history, 'games': records, 'evaluation': read_json(directory / 'evaluation.json'), 'evaluation_progress': read_json(directory / 'evaluation-progress.json'), 'checkpoint_timing': read_json(directory / 'checkpoint-timing.json'), 'load_timing': read_json(directory / 'load-timing.json')})
                valid = {str(p) for p in root.glob('[2-5]p*/games/*.json')}
                self.server.record_cache = {k: v for k, v in self.server.record_cache.items() if k in valid}
                examples_list = [{'name': p.stem, 'url': f'/api/example/{p.name}'} for p in sorted(examples.glob('*.json'))]
                return self.send_json({'now': time.time(), 'stop_requested': (root / 'STOP').exists(),
                                       'models': models, 'examples': examples_list, 'curriculum': read_json(root / 'curriculum.json')})
            match = re.fullmatch(r'/api/game/([2-5]p(?:-r[3-7])?)/(live|[\w-]+\.json)', path)
            if path == '/api/live':
                file = root / 'current.json'
            elif match:
                model, name = match.groups()
                file = root / model / ('live.json' if name == 'live' else 'games/' + name)
            elif re.fullmatch(r'/api/example/[\w.-]+\.json', path):
                file = examples / path.rsplit('/', 1)[-1]
            else:
                return self.send_error(404)
            # Only JSON in the configured roots, including when symlinks are present.
            if not (file.resolve().is_relative_to(root.resolve()) or file.resolve().is_relative_to(examples.resolve())):
                return self.send_error(404)
            try:
                return self.send_bytes(file.read_bytes(), 'application/json')
            except OSError:
                return self.send_error(404, 'No recording yet')

        def send_json(self, data):
            self.send_bytes(json.dumps(data).encode(), 'application/json')

        def send_bytes(self, data, mime):
            self.send_response(200)
            self.send_header('Content-Type', mime)
            self.send_header('Content-Length', str(len(data)))
            self.send_header('Cache-Control', 'no-store')
            self.send_header('X-Content-Type-Options', 'nosniff')
            self.end_headers()
            self.wfile.write(data)
    return Handler


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--checkpoint', type=Path, default=Path('checkpoints/organism'))
    parser.add_argument('--port', type=int, default=8765)
    args = parser.parse_args()
    server = ThreadingHTTPServer(('127.0.0.1', args.port), make_handler(args.checkpoint, Path('ogf')))
    server.record_cache = {}
    print(f'Organism dashboard: http://127.0.0.1:{args.port}', flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == '__main__':
    main()
