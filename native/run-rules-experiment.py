"""Equal-update baseline versus tightened-rules experiment for 2p/3 rings.

Arms play different games, so they cannot share a frozen opponent panel: every
anchor was trained under the original rules and any evaluation game has to be
played under one rule set or the other. Milestones therefore play the two arms
head to head twice, once under each rule set, and the intrinsic statistics of
each arm's own self-play carry the rest.
"""
import argparse, fcntl, json, os, shutil, subprocess, time
from pathlib import Path
import importlib.util
loader=importlib.util.spec_from_file_location('ablation',Path(__file__).with_name('run-ablation.py'))
ablation=importlib.util.module_from_spec(loader);loader.loader.exec_module(ablation)
write,digest,snapshot,integer=ablation.atomic,ablation.digest,ablation.snapshot,ablation.saved_integer

# Zero is the game as written; the tightened arm is the combination that moved
# the screen, since each rule alone left an escape route.
ARMS={'baseline': {'require-useful-action':0,'eat-threshold':0,'stall-limit':0,'sacrifice-yields-nothing':0},
      'tightened':{'require-useful-action':1,'eat-threshold':5,'stall-limit':8,'sacrifice-yields-nothing':1}}
RULESETS={'original':{'require_useful_action':False,'eat_threshold':0,'stall_limit':0,'sacrifice_yields_nothing':False},
          'tightened':{'require_useful_action':True,'eat_threshold':5,'stall_limit':8,'sacrifice_yields_nothing':True}}

def read(path,default=None):
 try:return json.loads(path.read_text())
 except FileNotFoundError:return default

def pin(source,target):
 target.unlink(missing_ok=True);os.link(source,target)

def prepare(args):
 root=args.root;file=root/'experiment.json'
 options={'rounds':args.rounds,'milestones':args.milestones,'games_per_seat':args.games_per_seat,
          'evaluation_max_steps':args.evaluation_max_steps,'cpu':args.cpu,'training':str(args.training)}
 if file.exists():
  spec=read(file)
  if spec['options']!=options:raise ValueError('Resume with the same experiment options')
  return spec
 source=args.training/'2p-r3';initial=root/'initial';initial.mkdir(parents=True,exist_ok=True)
 while True:
  generation=snapshot(source)
  try:
   for name in ['model.ot','adam.ot','state.json']:pin(generation/name,initial/name)
   break
  except FileNotFoundError:continue
 shutil.copy2(source/'config.json',initial/'config.json')
 if integer(initial/'state.json','training_step')!=0:raise ValueError('Initial snapshot must precede gradient updates')
 config=read(initial/'config.json')
 if (config['players'],config['rings'])!=(2,3):raise ValueError('This experiment is two-player, three rings')
 if any(config.get(k) for k in ['eat_threshold','require_useful_action','stall_limit','sacrifice_yields_nothing']):
  raise ValueError('Seed checkpoint already plays a tightened game; both arms must start from the same one')
 shutil.copy2('native/target/release/organism-train',root/'organism-train')
 spec={'options':options,'config':config,'arms':ARMS,'rulesets':RULESETS,
       'initial_iteration':integer(initial/'state.json','iteration'),
       'initial_hashes':{n:digest(initial/n) for n in ['model.ot','adam.ot','state.json']},
       'binary_sha256':digest(root/'organism-train')}
 for arm in ARMS:
  base=root/arm/'2p-r3';seed=base/'snapshots'/'initial';seed.mkdir(parents=True,exist_ok=True)
  for name in ['model.ot','adam.ot','state.json']:pin(initial/name,seed/name)
  shutil.copy2(initial/'model.ot',base/'baseline.ot');shutil.copy2(initial/'config.json',base/'config.json')
  write(base/'latest.json',{'generation':'initial'})
 write(file,spec)
 return spec

def halt(root,status):
 # A graceful stop used to return without touching status.json, leaving it
 # reading as though the arm it was part way through were still running.
 previous=read(root/'status.json',{}) or {}
 write(root/'status.json',{'stage':'stopped','reason':previous.get('reason','STOP requested'),
  'resume':'remove STOP from the experiment root and rerun the same command',
  'stopped_during':{k:v for k,v in status.items() if k!='stage'},'updated':time.time()})
 return False

def execute(root,command,stop,log,status):
 if (root/'STOP').exists():return halt(root,status)
 stop.unlink(missing_ok=True);write(root/'status.json',{'updated':time.time(),**status})
 with log.open('a') as output:
  p=subprocess.Popen(command,stdout=output,stderr=subprocess.STDOUT)
  try:
   while p.poll() is None:
    if shutil.disk_usage(root).free < 3*1024**3:
     (root/'STOP').touch();write(root/'status.json',{'stage':'stopping','reason':'Less than 3 GiB disk space free','updated':time.time()})
    if (root/'STOP').exists():stop.touch()
    time.sleep(.5)
  except BaseException:
   stop.touch();p.wait(timeout=180);raise
  if p.returncode:raise RuntimeError(f'Experiment process failed; see {log}')
 return True if not (root/'STOP').exists() else halt(root,status)

def main():
 parser=argparse.ArgumentParser(description=__doc__)
 parser.add_argument('--root',type=Path,default=Path('checkpoints/organism-rules-20260913'))
 parser.add_argument('--training',type=Path,default=Path('checkpoints/organism-native'))
 parser.add_argument('--rounds',type=int,default=30);parser.add_argument('--milestones',default='15,30')
 parser.add_argument('--games-per-seat',type=int,default=8)
 parser.add_argument('--evaluation-max-steps',type=int,default=1200)
 parser.add_argument('--cpu',action='store_true')
 args=parser.parse_args();args.root=args.root.absolute();args.training=args.training.absolute()
 args.milestones=[int(n) for n in args.milestones.split(',')]
 if args.rounds<1 or any(n<1 or n>args.rounds for n in args.milestones):raise ValueError('Invalid budgets')
 root=args.root;root.mkdir(parents=True,exist_ok=True)
 lock=(root/'runner.lock').open('w');fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 os.nice(10);spec=prepare(args);exe=root/'organism-train'
 if digest(exe)!=spec['binary_sha256']:raise ValueError('Frozen executable changed')
 config=spec['config'];names=list(spec['arms'])
 for round_number in range(1,args.rounds+1):
  order=names[(round_number-1)%len(names):]+names[:(round_number-1)%len(names)]
  for arm in order:
   base=root/arm;directory=base/'2p-r3';target=spec['initial_iteration']+round_number
   if integer(snapshot(directory)/'state.json','iteration')<target:
    command=[str(exe),'train','--players','2','--rings','3','--rings-2p','3','--checkpoint',str(base),
     '--iters','1','--eval-every','0','--concurrent-games',str(config['actors'] if args.cpu else 64),
     '--gpu-batch','32','--exploration-rounds','10','--threads','2','--duty','0.35','--vram-fraction','0.20',
     '--buffer',str(config['replay']),'--replay-game-cap','256','--cutoff-value','draw']
    for flag,key in [('blocks','blocks'),('filters','filters'),('sims','sims'),('actors','actors'),
                     ('max-steps','max_steps'),('repetition','repetition'),('batch-size','batch'),('train-steps','train_steps')]:
     command+=['--'+flag,str(config[key])]
    for flag,value in spec['arms'][arm].items():command+=['--'+flag,str(value)]
    if args.cpu:command+=['--cpu']
    if not execute(root,command,base/'STOP',base/'training.log',{'stage':'training','arm':arm,'round':round_number}):return
    if integer(snapshot(directory)/'state.json','iteration')!=target:raise RuntimeError('Training budget mismatch')
  if round_number in args.milestones:
   # One head-to-head per rule set: which model is stronger at which game.
   stage=root/f'milestone-{round_number:03d}';stage.mkdir(exist_ok=True)
   weights={}
   for arm in names:
    frozen=stage/f'{arm}.ot'
    if not frozen.exists():shutil.copy2(snapshot(root/arm/'2p-r3')/'model.ot',frozen)
    weights[arm]=frozen
   for ruleset,rules in spec['rulesets'].items():
    job=stage/ruleset;job.mkdir(exist_ok=True);manifest=job/'manifest.json'
    if not manifest.exists():
     write(manifest,{'config':{**config,**rules,'eval_max_steps':args.evaluation_max_steps},
      'candidate':{'identity':f'tightened-{round_number}','weights':str(weights['tightened']),'sha256':digest(weights['tightened'])},
      'opponent':{'identity':f'baseline-{round_number}','weights':str(weights['baseline']),'sha256':digest(weights['baseline'])},
      'simulations':config['sims'],'games_per_seat':args.games_per_seat,'seed':551013,'cutoff_value':'draw'})
    if not read(job/'report.json',{}).get('complete'):
     command=[str(exe),'compare',str(manifest)]+(['--cpu'] if args.cpu else [])
     if not execute(root,command,job/'STOP',job/'run.log',{'stage':'evaluation','ruleset':ruleset,'round':round_number}):return
  write(root/'progress.json',{'completed_rounds':round_number,'updates_per_arm':round_number*config['train_steps'],'updated':time.time()})
 write(root/'status.json',{'stage':'complete','updated':time.time()})

if __name__=='__main__':main()
