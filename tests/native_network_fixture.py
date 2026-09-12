"""Create a deterministic PyTorch oracle for native inference/Adam/resume checks."""
import json
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import torch
from alphazero.network import AlphaZeroNetwork

torch.set_num_threads(2)
torch.manual_seed(123)
out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)
net = AlphaZeroNetwork(34, 18, 71, num_res_blocks=1, num_filters=8, value_size=2)
torch.save(dict(net.state_dict()), out / 'initial.pt')
device = sys.argv[2] if len(sys.argv) > 2 else 'cpu'
net.to(device)
x = torch.randn(4, 34, 18, 18)
pi = torch.softmax(torch.randn(4, 71), -1)
v = torch.randn(4, 2).tanh()
x, pi, v = x.to(device), pi.to(device), v.to(device)
net.eval()
with torch.no_grad():
    p0, v0 = net(x)
fixture = dict(x=x.flatten().tolist(), pi=pi.flatten().tolist(), v=v.flatten().tolist(),
               policy=p0.exp().flatten().tolist(), value=v0.flatten().tolist())
optimizer = torch.optim.Adam(net.parameters(), lr=1e-3, weight_decay=1e-4)
net.train()
for _ in range(2):
    optimizer.zero_grad()
    p, pred = net(x)
    loss = -(p * pi).sum(-1).mean() + (pred-v).square().mean()
    loss.backward()
    torch.nn.utils.clip_grad_norm_(net.parameters(), 5.0)
    optimizer.step()
fixture['parameters'] = {k: t.flatten().tolist() for k, t in net.state_dict().items()
                         if not k.endswith('num_batches_tracked')}
(out / 'fixture.json').write_text(json.dumps(fixture))
print(out)
