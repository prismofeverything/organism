"""Same weights/positions/search budget, Python serial versus native batched MCTS."""
import json, sys, time
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
import numpy as np
import torch
from alphazero.games.organism.game import OrganismGame
from alphazero.network import AlphaZeroNetwork
from alphazero.mcts import MCTS

torch.set_num_threads(2)
torch.set_num_interop_threads(1)
torch.manual_seed(0)
g=OrganismGame(2,num_rings=4,remove_notches=False)
rng=np.random.default_rng(42)
s=g.initial_state(); states=[]; paths=[]; path=[]
for i in range(180):
    legal=g.legal_actions(s)
    a=int(rng.choice(list(legal)));path.append(a);s=legal[a]
    if i in (30,80,130,170): states.append(s);paths.append(path.copy())
root=Path('/tmp/organism-search-benchmark');root.mkdir(exist_ok=True)
net=AlphaZeroNetwork.for_game(g,num_res_blocks=4,num_filters=64).eval()
torch.save(dict(net.state_dict()),root/'weights.pt')
(root/'fixture.json').write_text(json.dumps({'weights':str(root/'weights.pt'),'paths':paths}))
net.cuda()
search=MCTS(g,net,num_simulations=64,dirichlet_eps=0)
for s in states:search.policy(s)
start=time.perf_counter()
for _ in range(3):
    for s in states:search.policy(s)
seconds=time.perf_counter()-start
print(json.dumps(dict(backend='python',decisions=12,seconds=seconds,decisions_per_second=12/seconds,simulations=64)))
