"""Historical-seat routing, learner-only replay, frozen weights and exact resume."""
import hashlib,json,subprocess,tempfile,shutil
from pathlib import Path
root=Path(tempfile.mkdtemp(prefix='organism-diversity-test-'))
exe=Path('native/target/release/organism-train').resolve()
def state(base):
 g=json.loads((base/'latest.json').read_text())['generation']
 return json.loads((base/'snapshots'/g/'state.json').read_text())
def run(flags):
 with (root/'run.log').open('a') as log:
  subprocess.run([str(exe),'train','--cpu',*flags],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=120)
for players in [2,3]:
 flags=['--players',str(players),'--actors','6','--concurrent-games','12','--sims','2','--max-steps','30','--blocks','1','--filters','8','--train-steps','1','--batch-size','8','--eval-every','0','--replay-game-cap','0','--buffer','2000','--cutoff-value','draw']
 source=root/f'initial-{players}'
 run([*flags,'--checkpoint',str(source),'--iters','1'])
 base=source/f'{players}p';g=json.loads((base/'latest.json').read_text())['generation'];weights=base/'snapshots'/g/'model.ot'
 pool=root/f'pool-{players}.json';pool.write_text(json.dumps([str(weights)]));original=hashlib.sha256(weights.read_bytes()).hexdigest()
 for branch in ['continuous','split']:
  target=root/f'{branch}-{players}';shutil.copytree(source,target)
  for _ in range(1 if branch=='continuous' else 2):
   run([*flags,'--checkpoint',str(target),'--iters','2' if branch=='continuous' else '1','--opponent-pool',str(pool)])
 a=state(root/f'continuous-{players}/{players}p');b=state(root/f'split-{players}/{players}p')
 for key in ['rng','replay','replay_pos','iteration']:
  assert a[key]==b[key],(players,key)
 records={}
 for p in (root/f'continuous-{players}/{players}p/games').glob('*.json'):
  d=json.loads(p.read_text());records[d['id']]=d['result']
 historical=[g for g,r in records.items() if r.get('learner_seat') is not None]
 assert historical,(players,'no historical games')
 for sample in a['replay']:
  result=records.get(sample['game_id'],{});seat=result.get('learner_seat')
  if seat is not None:assert sample['state']['player']==seat,(players,'opponent replay target leaked')
 assert hashlib.sha256(weights.read_bytes()).hexdigest()==original
 # All episodes may have finished; assignments remain in the recorded results.
 assert all(records[g]['opponent_index']==0 for g in historical)
print('2p/3p historical assignments, learner-only replay, immutable opponents and restart determinism passed:',root)
