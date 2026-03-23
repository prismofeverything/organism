"""AlphaZero training loop.

Alternates between:
  1. Self-play data generation (filling the replay buffer)
  2. Network update on mini-batches from the buffer

Losses:
  policy_loss = NLLLoss(log_pi_predicted, pi_target)
  value_loss  = MSELoss(v_predicted, v_target)
  total_loss  = policy_loss + value_loss

Checkpoints are saved to {checkpoint_dir}/{game_name}_iter_{n}.pt after
each training iteration.

Usage:
  from alphazero.games.journey.game import JourneyGame
  from alphazero.train import Trainer

  game = JourneyGame(num_players=5)
  trainer = Trainer(game, checkpoint_dir="checkpoints/journey")
  trainer.run(num_iterations=100)
"""

from __future__ import annotations
import os
import time
from typing import Any

import numpy as np
import torch
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import DataLoader, Dataset

from alphazero.games.base import Game
from alphazero.network import AlphaZeroNetwork
from alphazero.self_play import ReplayBuffer, self_play_game


# ── dataset ────────────────────────────────────────────────────────────────────

class SelfPlayDataset(Dataset):
    def __init__(self, samples: list[dict]):
        self.samples = samples

    def __len__(self):
        return len(self.samples)

    def __getitem__(self, idx: int) -> tuple:
        s = self.samples[idx]
        state  = torch.FloatTensor(s["state"])
        pi     = torch.FloatTensor(s["pi"])
        value  = torch.FloatTensor([s["value"]])
        return state, pi, value


# ── trainer ────────────────────────────────────────────────────────────────────

class Trainer:
    def __init__(
        self,
        game: Game,
        checkpoint_dir: str = "checkpoints",
        num_res_blocks: int = 10,
        num_filters: int = 128,
        lr: float = 1e-3,
        weight_decay: float = 1e-4,
        # Self-play params
        games_per_iteration: int = 50,
        mcts_simulations: int = 400,
        temperature_threshold: int = 30,
        max_steps_per_game: int = 400,
        # Training params
        replay_buffer_capacity: int = 100_000,
        min_buffer_size: int = 1_000,
        batch_size: int = 512,
        train_steps_per_iter: int = 200,
        device: str = "cpu",
    ):
        self.game = game
        self.checkpoint_dir = checkpoint_dir
        os.makedirs(checkpoint_dir, exist_ok=True)

        self.device = torch.device(device)
        print(f"Using device: {self.device}")

        self.network = AlphaZeroNetwork.for_game(
            game,
            num_res_blocks=num_res_blocks,
            num_filters=num_filters,
        ).to(self.device)

        self.optimizer = optim.Adam(
            self.network.parameters(), lr=lr, weight_decay=weight_decay
        )
        self.scheduler = optim.lr_scheduler.StepLR(self.optimizer, step_size=20, gamma=0.5)

        self.replay_buffer = ReplayBuffer(replay_buffer_capacity)

        self.games_per_iteration = games_per_iteration
        self.mcts_simulations = mcts_simulations
        self.temperature_threshold = temperature_threshold
        self.max_steps_per_game = max_steps_per_game
        self.min_buffer_size = min_buffer_size
        self.batch_size = batch_size
        self.train_steps_per_iter = train_steps_per_iter

        self.iteration = 0

    # ── main loop ──────────────────────────────────────────────────────────────

    def run(self, num_iterations: int):
        for i in range(num_iterations):
            self.iteration += 1
            t0 = time.time()
            print(f"\n=== Iteration {self.iteration} ===")

            # 1. Self-play
            self._generate_games()

            # 2. Train (once buffer is large enough)
            if len(self.replay_buffer) >= self.min_buffer_size:
                loss_p, loss_v = self._train_epoch()
                print(f"  policy_loss={loss_p:.4f}  value_loss={loss_v:.4f}")
            else:
                print(f"  Buffer size {len(self.replay_buffer)} < min {self.min_buffer_size}, skipping train")

            self.scheduler.step()

            # 3. Checkpoint
            path = os.path.join(
                self.checkpoint_dir,
                f"{self.game.name}_iter_{self.iteration:04d}.pt"
            )
            self.network.save(path)
            print(f"  Saved checkpoint: {path}  ({time.time() - t0:.1f}s)")

    # ── self-play ──────────────────────────────────────────────────────────────

    def _generate_games(self):
        self.network.eval()
        total_steps = 0
        for g in range(self.games_per_iteration):
            samples = self_play_game(
                self.game,
                self.network,
                num_simulations=self.mcts_simulations,
                temperature_threshold=self.temperature_threshold,
                max_steps=self.max_steps_per_game,
            )
            self.replay_buffer.push(samples)
            total_steps += len(samples)
        print(f"  Self-play: {self.games_per_iteration} games, {total_steps} steps → buffer={len(self.replay_buffer)}")

    # ── training ───────────────────────────────────────────────────────────────

    def _train_epoch(self) -> tuple[float, float]:
        self.network.train()
        samples = self.replay_buffer.sample(
            min(len(self.replay_buffer), self.batch_size * self.train_steps_per_iter)
        )
        dataset = SelfPlayDataset(samples)
        loader = DataLoader(dataset, batch_size=self.batch_size, shuffle=True, drop_last=False)

        total_p_loss = 0.0
        total_v_loss = 0.0
        steps = 0

        for state, pi, value in loader:
            state = state.to(self.device)
            pi    = pi.to(self.device)
            value = value.to(self.device)

            log_pi_pred, v_pred = self.network(state)

            # Policy loss: cross-entropy (pi is a soft target)
            p_loss = -(pi * log_pi_pred).sum(dim=1).mean()
            # Value loss: MSE
            v_loss = F.mse_loss(v_pred, value)

            loss = p_loss + v_loss
            self.optimizer.zero_grad()
            loss.backward()
            torch.nn.utils.clip_grad_norm_(self.network.parameters(), max_norm=5.0)
            self.optimizer.step()

            total_p_loss += p_loss.item()
            total_v_loss += v_loss.item()
            steps += 1

            if steps >= self.train_steps_per_iter:
                break

        return total_p_loss / max(steps, 1), total_v_loss / max(steps, 1)


# ── CLI entry-point ────────────────────────────────────────────────────────────

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Train AlphaZero on Journey")
    parser.add_argument("--players",  type=int, default=5)
    parser.add_argument("--iters",    type=int, default=100)
    parser.add_argument("--sims",     type=int, default=400)
    parser.add_argument("--games",    type=int, default=50)
    parser.add_argument("--blocks",   type=int, default=10)
    parser.add_argument("--filters",  type=int, default=128)
    parser.add_argument("--max-steps", type=int, default=400)
    parser.add_argument("--checkpoint", default="checkpoints/journey")
    args = parser.parse_args()

    from alphazero.games.journey.game import JourneyGame

    game = JourneyGame(num_players=args.players)
    trainer = Trainer(
        game,
        checkpoint_dir=args.checkpoint,
        num_res_blocks=args.blocks,
        num_filters=args.filters,
        games_per_iteration=args.games,
        mcts_simulations=args.sims,
        max_steps_per_game=args.max_steps,
    )
    trainer.run(num_iterations=args.iters)
