"""AlphaZero residual network — policy head + value head.

Architecture mirrors the original AlphaZero paper:
  - Input convolutional layer
  - N residual blocks (each: BN → ReLU → Conv → BN → ReLU → Conv + skip)
  - Policy head: Conv → BN → ReLU → Linear → log-softmax
  - Value head:  Conv → BN → ReLU → Linear → ReLU → Linear → tanh

The network is game-agnostic: it only needs the input shape and action size.

Usage:
  game = JourneyGame()
  state = game.initial_state()
  player = game.current_player(state)
  tensor = game.encode_state(state, player)   # (C, H, W) or (D,)
  net = AlphaZeroNetwork.for_game(game)
  log_pi, value = net(torch.FloatTensor(tensor).unsqueeze(0))
"""

from __future__ import annotations
from typing import Sequence

import torch
import torch.nn as nn
import torch.nn.functional as F


class ResidualBlock(nn.Module):
    def __init__(self, channels: int):
        super().__init__()
        self.conv1 = nn.Conv2d(channels, channels, 3, padding=1, bias=False)
        self.bn1   = nn.BatchNorm2d(channels)
        self.conv2 = nn.Conv2d(channels, channels, 3, padding=1, bias=False)
        self.bn2   = nn.BatchNorm2d(channels)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        residual = x
        out = F.relu(self.bn1(self.conv1(x)))
        out = self.bn2(self.conv2(out))
        return F.relu(out + residual)


class AlphaZeroNetwork(nn.Module):
    """Dual-head residual network.

    Args:
        input_channels: C dimension of the state tensor.
        board_size: H (= W) of the spatial grid, or 1 for flat encodings.
        action_size: number of actions in the policy head.
        num_res_blocks: depth of the tower.
        num_filters: width of residual tower.
    """

    def __init__(
        self,
        input_channels: int,
        board_size: int,
        action_size: int,
        num_res_blocks: int = 10,
        num_filters: int = 128,
    ):
        super().__init__()
        self.board_size = board_size
        self.action_size = action_size

        # Input
        self.input_conv = nn.Conv2d(input_channels, num_filters, 3, padding=1, bias=False)
        self.input_bn   = nn.BatchNorm2d(num_filters)

        # Residual tower
        self.res_blocks = nn.ModuleList([ResidualBlock(num_filters) for _ in range(num_res_blocks)])

        flat_size = num_filters * board_size * board_size

        # Policy head
        self.policy_conv = nn.Conv2d(num_filters, 2, 1, bias=False)
        self.policy_bn   = nn.BatchNorm2d(2)
        self.policy_fc   = nn.Linear(2 * board_size * board_size, action_size)

        # Value head
        self.value_conv = nn.Conv2d(num_filters, 1, 1, bias=False)
        self.value_bn   = nn.BatchNorm2d(1)
        self.value_fc1  = nn.Linear(board_size * board_size, 256)
        self.value_fc2  = nn.Linear(256, 1)

    def forward(self, x: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        # x: (B, C, H, W)
        out = F.relu(self.input_bn(self.input_conv(x)))
        for block in self.res_blocks:
            out = block(out)

        # Policy
        p = F.relu(self.policy_bn(self.policy_conv(out)))
        p = p.view(p.size(0), -1)
        p = F.log_softmax(self.policy_fc(p), dim=1)   # log-softmax for NLLLoss

        # Value
        v = F.relu(self.value_bn(self.value_conv(out)))
        v = v.view(v.size(0), -1)
        v = F.relu(self.value_fc1(v))
        v = torch.tanh(self.value_fc2(v))

        return p, v

    @classmethod
    def for_game(cls, game, **kwargs) -> "AlphaZeroNetwork":
        """Construct network sized to match a Game's encoding."""
        # Probe encoding shape with a dummy state
        dummy = game.initial_state()
        player = game.current_player(dummy)
        tensor = game.encode_state(dummy, player)
        if tensor.ndim == 3:
            C, H, W = tensor.shape
            assert H == W, "Only square grids supported"
            return cls(C, H, game.action_space_size(), **kwargs)
        else:
            # Flat encoding — treat as (D, 1, 1) with 1×1 board
            D = tensor.shape[0]
            return cls(D, 1, game.action_space_size(), **kwargs)

    def save(self, path: str):
        torch.save(self.state_dict(), path)

    def load(self, path: str, device: str = "cpu"):
        self.load_state_dict(torch.load(path, map_location=device))
        return self
