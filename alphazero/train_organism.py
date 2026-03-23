"""Train AlphaZero on the Organism board game.

Usage:
  python -m alphazero.train_organism [options]

Example (quick test):
  python -m alphazero.train_organism --iters 10 --sims 20 --games 5 --max-steps 200

Example (full run):
  python -m alphazero.train_organism --iters 100 --sims 400 --games 50 --max-steps 400
"""

import argparse

from alphazero.games.organism.game import OrganismGame
from alphazero.train import Trainer

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Train AlphaZero on Organism")
    parser.add_argument("--players",    type=int, default=5)
    parser.add_argument("--iters",      type=int, default=100)
    parser.add_argument("--sims",       type=int, default=400)
    parser.add_argument("--games",      type=int, default=50)
    parser.add_argument("--blocks",     type=int, default=10)
    parser.add_argument("--filters",    type=int, default=128)
    parser.add_argument("--max-steps",  type=int, default=400)
    parser.add_argument("--checkpoint", default="checkpoints/organism")
    parser.add_argument("--device",     default="cpu")
    args = parser.parse_args()

    game = OrganismGame(num_players=args.players)
    trainer = Trainer(
        game,
        checkpoint_dir=args.checkpoint,
        num_res_blocks=args.blocks,
        num_filters=args.filters,
        games_per_iteration=args.games,
        mcts_simulations=args.sims,
        max_steps_per_game=args.max_steps,
        device=args.device,
    )
    trainer.run(num_iterations=args.iters)
