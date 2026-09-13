"""Equal-update pure-self-play versus historical-opponent experiment for 2p/3p."""
import argparse, fcntl, hashlib, json, os, shutil, subprocess, time
from pathlib import Path
import importlib.util
loader=importlib.util.spec_from_file_location('ablation',Path(__file__).with_name('run-ablation.py'))
ablation=importlib.util.module_from_spec(loader);loader.loader.exec_module(ablation)
write,digest,snapshot,integer=ablation.atomic,ablation.digest,ablation.snapshot,ablation.saved_integer

def read(path,default=None):
 try:return json.loads(path.read_text())
 except FileNotFoundError:return default

def pin(source, target):
 # Native generation files are immutable: hard links retain the exact snapshot
 # through pruning without duplicating its large replay/unfinished-game payload.
 target.unlink(missing_ok=True)
 os.link(source, target)

def prepare(args):
 root=args.root;file=root/'experiment.json'
 options={'rounds':args.rounds,'milestones':args.milestones,'games_per_seat':args.games_per_seat,'cpu':args.cpu,'training':str(args.training),'panel':str(args.panel)}
 if file.exists():
  spec=read(file)
  if spec['options']!=options:raise ValueError('Resume with the same experiment options')
  return spec
 panel=read(args.panel)
 pools=read(args.training_pools) if args.training_pools else {
  '2p-r3':[str(Path('checkpoints/model-rollbacks/20260913-101341/2p-r3/opponent-archive')/f'{i:09d}.ot') for i in [540,785]],
  '3p':[str(args.training/'3p/opponent-archive'/f'{i:09d}.ot') for i in [345,605]],
 }
 spec={'options':options,'models':{},'binary_sha256':digest(Path('native/target/release/organism-train'))}
 shutil.copy2('native/target/release/organism-train',root/'organism-train')
 for model in ['2p-r3','3p']:
  source=args.training/model;initial=root/'initial'/model;initial.mkdir(parents=True,exist_ok=True)
  while True:
   generation=snapshot(source)
   try:
    for name in ['model.ot','adam.ot','state.json']:pin(generation/name,initial/name)
    break
   except FileNotFoundError:continue
  shutil.copy2(source/'config.json',initial/'config.json')
  if integer(initial/'state.json','training_step')!=0:raise ValueError('Initial snapshot must precede gradient updates')
  config=read(initial/'config.json');iteration=integer(initial/'state.json','iteration')
  # Match each current production recipe; only history mixing differs within its pair.
  settings={'buffer':config['replay'],'cap':0 if config['players']==2 else 256,'cutoff':'draw' if config['players']==2 else 'mask'}
  frozen=[]
  for i,path in enumerate(pools[model]):
   dest=initial/f'training-opponent-{i}.ot';shutil.copy2(path,dest);frozen.append({'weights':str(dest),'sha256':digest(dest)})
  write(initial/'pool.json',[p['weights'] for p in frozen])
  opponents=[]
  for i,opponent in enumerate(panel['opponents'][model]):
   dest=initial/f'benchmark-opponent-{i}.ot';shutil.copy2(opponent['weights'],dest)
   if digest(dest)!=opponent['sha256']:raise ValueError('Benchmark weight checksum mismatch')
   opponents.append({**opponent,'weights':str(dest)})
  if {p['sha256'] for p in frozen}&{p['sha256'] for p in opponents}:raise ValueError('Training and benchmark opponents must be disjoint')
  hashes={n:digest(initial/n) for n in ['model.ot','adam.ot','state.json']}
  spec['models'][model]={'config':config,'initial_iteration':iteration,'settings':settings,'initial_hashes':hashes,'training_opponents':frozen,'benchmark_opponents':opponents}
  for arm in ['self_play','history_mix']:
   base=root/model/arm/model;seed=base/'snapshots'/'initial';seed.mkdir(parents=True,exist_ok=True)
   for name in hashes:pin(initial/name,seed/name)
   shutil.copy2(initial/'model.ot',base/'baseline.ot');shutil.copy2(initial/'config.json',base/'config.json')
   write(base/'latest.json',{'generation':'initial'})
 write(file,spec)
 return spec

def execute(root,command,stop,log,status):
 if (root/'STOP').exists():return False
 stop.unlink(missing_ok=True);write(root/'status.json',{'updated':time.time(),**status})
 with log.open('a') as output:
  p=subprocess.Popen(command,stdout=output,stderr=subprocess.STDOUT)
  try:
   while p.poll() is None:
    if shutil.disk_usage(root).free < 3 * 1024**3:
     (root/'STOP').touch();write(root/'status.json',{'stage':'stopping','reason':'Less than 3 GiB disk space free','updated':time.time()})
    if (root/'STOP').exists():stop.touch()
    time.sleep(.5)
  except BaseException:
   stop.touch();p.wait(timeout=180);raise
  if p.returncode:raise RuntimeError(f'Experiment process failed; see {log}')
 return not (root/'STOP').exists()

def main():
 parser=argparse.ArgumentParser(description=__doc__)
 parser.add_argument('--root',type=Path,default=Path('checkpoints/organism-diversity-20260913'))
 parser.add_argument('--training',type=Path,default=Path('checkpoints/organism-native'))
 parser.add_argument('--panel',type=Path,default=Path('checkpoints/organism-benchmark-20260913/protocol.json'))
 parser.add_argument('--training-pools',type=Path)
 parser.add_argument('--rounds',type=int,default=50);parser.add_argument('--milestones',default='20,50')
 parser.add_argument('--games-per-seat',type=int,default=16);parser.add_argument('--cpu',action='store_true')
 args=parser.parse_args();args.root=args.root.absolute();args.training=args.training.absolute();args.panel=args.panel.absolute()
 args.milestones=[int(n) for n in args.milestones.split(',')]
 if args.rounds<1 or args.games_per_seat<1 or any(n<1 or n>args.rounds for n in args.milestones):raise ValueError('Invalid budgets')
 root=args.root;root.mkdir(parents=True,exist_ok=True);lock=(root/'runner.lock').open('w');fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 os.nice(10);spec=prepare(args);exe=root/'organism-train'
 if digest(exe)!=spec['binary_sha256']:raise ValueError('Frozen executable changed')
 for model,setup in spec['models'].items():
  for item in setup['training_opponents']+setup['benchmark_opponents']:
   if digest(Path(item['weights']))!=item['sha256']:raise ValueError('Frozen opponent changed')
 order=[(m,a) for m in spec['models'] for a in ['self_play','history_mix']]
 for round_number in range(1,args.rounds+1):
  rotated=order[(round_number-1)%len(order):]+order[:(round_number-1)%len(order)]
  for model,arm in rotated:
   setup=spec['models'][model];config=setup['config'];settings=setup['settings'];base=root/model/arm;directory=base/model
   target=setup['initial_iteration']+round_number
   if integer(snapshot(directory)/'state.json','iteration')<target:
    command=[str(exe),'train','--players',str(config['players']),'--rings',str(config['rings']),'--rings-2p',str(config['rings']),
     '--checkpoint',str(base),'--iters','1','--eval-every','0','--concurrent-games',str(config['actors'] if args.cpu else 64),
     '--gpu-batch','32','--exploration-rounds','10','--threads','2','--duty','0.35','--vram-fraction','0.20',
     '--buffer',str(settings['buffer']),'--replay-game-cap',str(settings['cap']),'--cutoff-value',settings['cutoff']]
    for flag,key in [('blocks','blocks'),('filters','filters'),('sims','sims'),('actors','actors'),('max-steps','max_steps'),('repetition','repetition'),('batch-size','batch'),('train-steps','train_steps')]:command+=['--'+flag,str(config[key])]
    if arm=='history_mix':command+=['--opponent-pool',str(root/'initial'/model/'pool.json')]
    if args.cpu:command+=['--cpu']
    if not execute(root,command,base/'STOP',base/'training.log',{'stage':'training','model':model,'arm':arm,'round':round_number}):return
    if integer(snapshot(directory)/'state.json','iteration')!=target:raise RuntimeError('Training budget mismatch')
   if round_number in args.milestones:
    evaluation=base/f'evaluation-{round_number:03d}';evaluation.mkdir(exist_ok=True)
    candidate=evaluation/'candidate.ot'
    if not candidate.exists():
     if integer(snapshot(directory)/'state.json','iteration')!=target:raise RuntimeError('Missing milestone checkpoint')
     shutil.copy2(snapshot(directory)/'model.ot',candidate)
    for opponent in setup['benchmark_opponents']:
     job=evaluation/opponent['identity'];job.mkdir(exist_ok=True);manifest=job/'manifest.json'
     if not manifest.exists():write(manifest,{'config':config,'candidate':{'identity':f'{model}-{arm}-{target}','weights':str(candidate),'sha256':digest(candidate)},'opponent':opponent,'simulations':config['sims'],'games_per_seat':args.games_per_seat,'seed':830013,'cutoff_value':'draw'})
     if not read(job/'report.json',{}).get('complete'):
      command=[str(exe),'compare',str(manifest)]+(['--cpu'] if args.cpu else [])
      if not execute(root,command,job/'STOP',job/'run.log',{'stage':'evaluation','model':model,'arm':arm,'round':round_number,'opponent':opponent['identity']}):return
  write(root/'progress.json',{'completed_rounds':round_number,'updates_per_arm':{m:round_number*s['config']['train_steps'] for m,s in spec['models'].items()},'updated':time.time()})
 write(root/'status.json',{'stage':'complete','updated':time.time()})

if __name__=='__main__':main()
