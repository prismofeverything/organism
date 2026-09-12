"""Resumable Organism self-play; see alphazero/README.md."""
import argparse
import gc
import json
import os
from pathlib import Path
import signal
import time

import torch

from alphazero.games.organism.game import OrganismGame
from alphazero.train import Trainer


class StopTraining(Exception):
    pass


class BackgroundControl:
    """Cooperative stop and approximate wall-clock duty cycle, between operations."""
    def __init__(self, stop_file, duty, device):
        self.stop_file = Path(stop_file)
        self.duty = duty
        self.device = device
        self.stopping = False
        self.last = time.monotonic()

    def stop(self, *_):
        self.stopping = True

    def __call__(self):
        if self.stopping or self.stop_file.exists():
            raise StopTraining()
        if self.device == "cuda":
            torch.cuda.synchronize()
        elapsed = time.monotonic() - self.last
        rest = min(elapsed * (1 / self.duty - 1), 5.0)
        deadline = time.monotonic() + rest
        while time.monotonic() < deadline:
            if self.stopping or self.stop_file.exists():
                raise StopTraining()
            time.sleep(min(0.1, max(0, deadline - time.monotonic())))
        self.last = time.monotonic()


def positive(value):
    result = int(value)
    if result < 1:
        raise argparse.ArgumentTypeError("must be positive")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--players", type=int, choices=[2, 3, 4, 5], nargs="+", default=[2, 3])
    parser.add_argument("--rings", type=int, choices=range(4, 8), help="default: 4 for two/three players, 7 otherwise")
    parser.add_argument("--no-notches", action="store_true")
    parser.add_argument("--iters", type=positive, default=1, help="cycles per model (unless --forever)")
    parser.add_argument("--forever", action="store_true")
    parser.add_argument("--sims", type=positive, default=64)
    parser.add_argument("--games", type=positive, default=4)
    parser.add_argument("--blocks", type=positive, default=4)
    parser.add_argument("--filters", type=positive, default=64)
    parser.add_argument("--max-steps", type=positive, default=4000)
    parser.add_argument("--batch-size", type=positive, default=32)
    parser.add_argument("--train-steps", type=positive, default=100)
    parser.add_argument("--buffer", type=positive, default=5000)
    parser.add_argument("--min-buffer", type=positive, default=256)
    parser.add_argument("--threads", type=positive, default=2)
    parser.add_argument("--duty", type=float, default=0.35)
    parser.add_argument("--vram-fraction", type=float, default=0.35)
    parser.add_argument("--checkpoint", default="checkpoints/organism")
    parser.add_argument("--warm-start", type=Path, help="root containing 2p/3p model exports; initializes new runs only")
    parser.add_argument("--stop-file", help="default: CHECKPOINT/STOP")
    parser.add_argument("--device", choices=["cuda", "cpu"], default="cuda")
    parser.add_argument("--repetition", type=int, default=3, help="end an episode on this occurrence of a state; 0 disables")
    parser.add_argument("--truncation", choices=["discard", "draw", "shaped"], default="draw",
                        help="draw: zero cutoff targets; discard: learn only from finished games; shaped: heuristic bootstrap")
    args = parser.parse_args()
    if not 0 < args.duty <= 1 or not 0 < args.vram_fraction <= 1:
        parser.error("duty and vram-fraction must be in (0, 1]")
    if args.repetition < 0 or args.repetition == 1:
        parser.error("repetition must be 0 (disabled) or at least 2")
    if args.min_buffer > args.buffer:
        parser.error("min-buffer must not exceed buffer")
    root = Path(args.checkpoint)
    root.mkdir(parents=True, exist_ok=True)
    if args.stop_file is None:
        args.stop_file = str(root / "STOP")
    # Prevent two trainers from corrupting the same snapshots.
    import fcntl
    lock = (root / "training.lock").open("w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        parser.error("another trainer is using this checkpoint directory")
    if Path(args.stop_file).exists():
        print(f"Stop file exists: {args.stop_file}. Remove it before restarting.")
        return
    torch.set_num_threads(args.threads)
    torch.set_num_interop_threads(1)
    os.nice(10)
    if args.device == "cuda":
        if not torch.cuda.is_available():
            parser.error("CUDA unavailable; check NVIDIA driver and CUDA PyTorch installation")
        torch.cuda.set_per_process_memory_fraction(args.vram_fraction)
        print(f"GPU: {torch.cuda.get_device_name(0)}", flush=True)
    control = BackgroundControl(args.stop_file, args.duty, args.device)
    signal.signal(signal.SIGINT, control.stop)
    signal.signal(signal.SIGTERM, control.stop)
    cycle = 0
    while args.forever or cycle < args.iters:
        for players in dict.fromkeys(args.players):
            if control.stopping or Path(args.stop_file).exists():
                return
            directory = root / f"{players}p"
            directory.mkdir(exist_ok=True)
            rings = args.rings or (4 if players <= 3 else 7)
            notches = rings > 4 and players != 4 and not args.no_notches
            config = {"version": 1, "players": players, "blocks": args.blocks,
                      "filters": args.filters, "truncation": args.truncation,
                      "buffer": args.buffer, "repetition": args.repetition, "rings": rings, "notches": notches}
            config_path = directory / "config.json"
            if config_path.exists() and json.loads(config_path.read_text()) != config:
                parser.error(f"incompatible configuration in {directory}; use a new checkpoint directory")
            config_path.write_text(json.dumps(config, indent=2) + "\n")
            trainer = Trainer(
                OrganismGame(num_players=players, num_rings=rings,
                             remove_notches=notches), checkpoint_dir=str(directory),
                num_res_blocks=args.blocks, num_filters=args.filters,
                games_per_iteration=args.games, mcts_simulations=args.sims,
                max_steps_per_game=args.max_steps, batch_size=args.batch_size,
                train_steps_per_iter=args.train_steps, replay_buffer_capacity=args.buffer,
                min_buffer_size=args.min_buffer, device=args.device,
                control=control, truncation=args.truncation, repetition_limit=args.repetition,
                record_games=True,
            )
            if not (directory / "latest.pt").exists() and args.warm_start:
                source = args.warm_start / f"{players}p"
                exports = sorted(source.glob(f"organism_{players}p_iter_*.pt"))
                if not exports:
                    parser.error(f"no weight exports in {source}")
                trainer.network.load(str(exports[-1]), args.device)
                print(f"Initialized weights from {exports[-1]}; starting fresh replay and optimizer")
            trainer.resume()
            control.last = time.monotonic()
            try:
                trainer.run(1)
            except StopTraining:
                trainer.save_training()
                if trainer.recorder:
                    trainer.recorder.finish({"terminal": False, "termination": "interrupted", "winner": None,
                                             "steps": len(trainer.recorder.data['frames']) - 1})
                    trainer.recorder.status("stopped")
                print("Saved training state; exiting to release GPU memory.", flush=True)
                return
            # Keep five model exports plus the resumable snapshot per player count.
            exports = sorted(directory.glob(f"organism_{players}p_iter_*.pt"))
            for old in exports[:-5]:
                old.unlink()
            del trainer
            gc.collect()
            if args.device == "cuda":
                torch.cuda.empty_cache()
        cycle += 1


if __name__ == "__main__":
    main()
