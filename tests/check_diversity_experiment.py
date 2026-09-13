"""Tiny end-to-end four-branch experiment and idempotent orchestration resume."""
import json,subprocess,tempfile,hashlib
from pathlib import Path
root=Path(tempfile.mkdtemp(prefix='diversity-orchestrator-'));training=root/'training';exe=Path('native/target/release/organism-train').resolve()
with (root/'run.log').open('w') as log:
 subprocess.run([str(exe),'train','--cpu','--players','2,3','--rings-2p','3','--actors','2','--sims','2','--max-steps','20','--blocks','1','--filters','8','--train-steps','1','--batch-size','8','--eval-every','0','--checkpoint',str(training)],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=90)
 pools={};panel={'opponents':{}}
 for model in ['2p-r3','3p']:
  base=training/model;g=json.loads((base/'latest.json').read_text())['generation'];weights=base/'snapshots'/g/'model.ot';baseline=base/'baseline.ot'
  pools[model]=[str(weights)];panel['opponents'][model]=[{'identity':'held-out-initial','weights':str(baseline),'sha256':hashlib.sha256(baseline.read_bytes()).hexdigest()}]
 (root/'pools.json').write_text(json.dumps(pools));(root/'panel.json').write_text(json.dumps(panel))
 command=['python3','native/run-diversity-experiment.py','--cpu','--root',str(root/'experiment'),'--training',str(training),'--panel',str(root/'panel.json'),'--training-pools',str(root/'pools.json'),'--rounds','1','--milestones','1','--games-per-seat','1']
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=120)
 reports=list((root/'experiment').glob('*/ */evaluation-*/ */report.json'.replace(' ','')))
 assert len(reports)==4,len(reports)
 hashes={p:hashlib.sha256(p.read_bytes()).hexdigest() for p in reports}
 for p in reports:assert json.loads(p.read_text())['complete']
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=120)
 assert hashes=={p:hashlib.sha256(p.read_bytes()).hexdigest() for p in reports}
 assert json.loads((root/'experiment/progress.json').read_text())['updates_per_arm']=={'2p-r3':1,'3p':1}
print('Four branches, equal updates, held-out evaluation and idempotent resume passed:',root)
