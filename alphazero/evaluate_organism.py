"""Seat-balanced evaluation against random play or a frozen checkpoint."""
import argparse
import json
from pathlib import Path

import numpy as np
import torch

from alphazero.games.organism.game import OrganismGame
from alphazero.mcts import MCTS
from alphazero.network import AlphaZeroNetwork


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("checkpoint", type=Path, help="2p or 3p checkpoint directory")
    p.add_argument("--opponent", type=Path, help="frozen model export; default random")
    p.add_argument("--games-per-seat", type=int, default=4)
    p.add_argument("--sims", type=int, default=64)
    p.add_argument("--max-steps", type=int, default=2400)
    p.add_argument("--device", choices=["cpu", "cuda"], default="cpu")
    args = p.parse_args()
    if min(args.games_per_seat, args.sims, args.max_steps) < 1:
        p.error("games-per-seat, sims and max-steps must be positive")
    torch.set_num_threads(2)
    np.random.seed(2026)
    config = json.loads((args.checkpoint / "config.json").read_text())
    game = OrganismGame(config["players"], num_rings=config["rings"],
                        remove_notches=config["notches"])
    def network():
        return AlphaZeroNetwork.for_game(game, num_res_blocks=config["blocks"],
                                         num_filters=config["filters"]).to(args.device).eval()
    candidate = network()
    data = torch.load(args.checkpoint / "latest.pt", map_location=args.device, weights_only=False)
    candidate.load_state_dict(data["network"])
    search = MCTS(game, candidate, num_simulations=args.sims, dirichlet_eps=0)
    opponent = None
    if args.opponent:
        opponent_net = network().load(str(args.opponent), args.device)
        opponent = MCTS(game, opponent_net, num_simulations=args.sims, dirichlet_eps=0)
    results = {"iteration": data["iteration"], "wins": 0, "losses": 0, "unfinished": 0,
               "games": game.num_players * args.games_per_seat, "repetition": 0, "max_steps": 0}
    for seat in range(game.num_players):
        for _ in range(args.games_per_seat):
            state = game.initial_state()
            player = state["turn_order"][seat]
            seen = {}
            cutoff = "max_steps"
            for step in range(args.max_steps):
                if game.is_terminal(state):
                    break
                key = game.repetition_key(state)
                seen[key] = seen.get(key, 0) + 1
                if config["repetition"] and seen[key] >= config["repetition"]:
                    cutoff = "repetition"
                    break
                legal = game.legal_actions(state)
                if not legal:
                    break
                policy = search if game.current_player(state) == player else opponent
                action = policy.best_action(state) if policy else int(np.random.choice(list(legal)))
                state = legal[action]
            if not game.is_terminal(state):
                results["unfinished"] += 1
                results[cutoff] += 1
            elif state["state"]["winner"] == player:
                results["wins"] += 1
            else:
                results["losses"] += 1
            print(json.dumps(results), flush=True)
    print("Final:", json.dumps(results))


if __name__ == "__main__":
    main()
