"""A handed-over position must play the same as one replayed from the opening.

The website has no move history to replay — it keeps an undo stack of whole
states — so its bot hands `organism-train serve` a position instead. This walks
a game both ways at once and checks the two descriptions never diverge.

    python3 tests/check_serve_position.py [--weights PATH] [--games N]
"""
import argparse, json, os, random, subprocess, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_WEIGHTS = ROOT / "checkpoints/organism-native/3p/serve.ot"


def serve(weights, players, rings, sims):
    env = dict(os.environ)
    torch = ROOT / ".venv-training/lib/python3.12/site-packages/torch/lib"
    env["LD_LIBRARY_PATH"] = f"{torch}:{env.get('LD_LIBRARY_PATH', '')}"
    command = [str(ROOT / "native/target/release/organism-train"), "serve",
               "--players", str(players), "--rings", str(rings),
               "--blocks", "8", "--filters", "128", "--sims", str(sims),
               "--threads", "4", "--cpu", "--weights", str(weights)]
    return subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            text=True, env=env, cwd=ROOT)


def main():
    parse = argparse.ArgumentParser()
    parse.add_argument("--weights", default=str(DEFAULT_WEIGHTS))
    parse.add_argument("--games", type=int, default=1)
    parse.add_argument("--sims", type=int, default=16)
    parse.add_argument("--steps", type=int, default=400)
    options = parse.parse_args()
    if not Path(options.weights).exists():
        print(f"no weights at {options.weights}; run native/publish-bot.sh first")
        return 0

    child = serve(options.weights, 3, 4, options.sims)

    def ask(request):
        child.stdin.write(json.dumps(request) + "\n")
        child.stdin.flush()
        line = child.stdout.readline()
        if not line:
            raise RuntimeError("move server exited")
        return json.loads(line)

    checked = 0
    try:
        for game in range(options.games):
            rng = random.Random(game)
            actions = []
            for step in range(options.steps):
                replayed = ask({"actions": actions, "echo": True})
                if replayed["action"] is None:
                    break
                direct = ask({"position": replayed["position"]})
                context = f"game {game} step {step}"
                assert direct["legal"] == replayed["legal"], f"{context}: legal moves differ"
                assert direct["action"] == replayed["action"], f"{context}: chose differently"
                assert direct["player"] == replayed["player"], f"{context}: seat differs"
                assert direct["round"] == replayed["round"], f"{context}: round differs"
                assert direct["phase"] == replayed["phase"], f"{context}: phase differs"
                checked += 1
                actions.append(rng.choice(replayed["legal"]))
        print(f"serve position parity passed: {checked} positions played the same "
              f"handed over as replayed")
    finally:
        child.stdin.close()
        child.wait(timeout=20)
    return 0


if __name__ == "__main__":
    sys.exit(main())
