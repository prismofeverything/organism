"""Small JSON observations for local monitoring; no tensor/checkpoint loading."""
import json
import os
from pathlib import Path
import time
import uuid
from pieces.organism_format import ring_label, ring_palette, COORDINATES


def space_id(space):
    return f'{ring_label(int(space[0]))}{space[1]}'


def atomic_json(path, data):
    path = Path(path)
    tmp = path.with_suffix(path.suffix + '.tmp')
    tmp.write_text(json.dumps(data, separators=(',', ':')))
    os.replace(tmp, path)


class GameRecorder:
    def __init__(self, directory, game, iteration, number):
        self.directory = Path(directory)
        self.replays = self.directory / 'games'
        self.replays.mkdir(exist_ok=True)
        self.game = game
        self.started = time.time()
        self.identity = f'{iteration:06d}-{number:02d}-{uuid.uuid4().hex[:8]}'
        self.data = {
            'format': 'organism', 'version': 2, 'profile': 'view', 'name': f'{game.name}-{self.identity}',
            'id': self.identity, 'iteration': iteration, 'number': number,
            'started': self.started, 'players': game._turn_order,
            'symmetry': game.symmetry,
            'board': {'center': space_id(game.center),
                      'ring-colors': ring_palette(game.num_rings),
                      'coordinates': COORDINATES,
                      **({'palette-tail': ring_palette(len(game._turn_order))[game.num_rings:]} if len(game._turn_order)>game.num_rings else {}),
                      'spaces': [space_id(s) for s in game.all_spaces],
                      'adjacencies': {space_id(s): [space_id(a) for a in adj]
                                      for s, adj in game.adjacencies.items()}},
            'homes': {p: [space_id(s) for s in info['starting_spaces']] for p, info in game._player_info},
            'source': 'alphazero', 'frame-unit': 'decision',
            'frames': [], 'result': None,
        }
        self.last_write = 0

    def observe(self, state, step, action=None):
        inner = state['state']
        from alphazero.games.organism.choices import find_state
        phase, _ = find_state(state)
        frame = {
            'turn': step, 'step': step, 'action': action, 'player': self.game.current_player(state),
            'phase': phase, 'round': inner.get('round'),
            'elements': [[e['player'], e['type'], space_id(s), e.get('food', 0)] for s, e in inner['elements'].items()],
            'food': {space_id(s): n for s, n in inner.get('food', {}).items()},
            'captures': {p: len(v) for p, v in inner.get('captures', {}).items()},
            'winner': inner.get('winner'),
        }
        self.data['frames'].append(frame)
        now = time.time()
        if now - self.last_write >= 2:
            atomic_json(self.directory / 'live.json', self.data)
            self.status('self_play', step=step, player=frame['player'], phase=phase)
            self.last_write = now

    def status(self, stage, **extra):
        atomic_json(self.directory / 'status.json', {
            'stage': stage, 'updated': time.time(), 'pid': os.getpid(),
            'iteration': self.data['iteration'], 'game_number': self.data['number'],
            'game_id': self.identity, 'started': self.started, **extra,
        })

    def finish(self, result):
        self.data['result'] = result
        self.data['finished'] = time.time()
        atomic_json(self.replays / f'{self.identity}.json', self.data)
        atomic_json(self.directory / 'live.json', self.data)
        # Bound disk use; these are viewing histories, separate from learning replay.
        for old in sorted(self.replays.glob('*.json'))[:-100]:
            old.unlink()
