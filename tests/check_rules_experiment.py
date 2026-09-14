"""Tiny end-to-end rules ablation: equal updates, both rule sets, idempotent resume."""
import json,subprocess,tempfile
from pathlib import Path
root=Path(tempfile.mkdtemp(prefix='rules-orchestrator-'));training=root/'training'
exe=Path('native/target/release/organism-train').resolve()
with (root/'run.log').open('w') as log:
 subprocess.run([str(exe),'train','--cpu','--players','2','--rings-2p','3','--actors','2','--sims','2',
  '--max-steps','20','--blocks','1','--filters','8','--train-steps','1','--batch-size','8',
  '--eval-every','0','--checkpoint',str(training)],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=90)
 command=['python3','native/run-rules-experiment.py','--cpu','--root',str(root/'experiment'),
  '--training',str(training),'--rounds','1','--milestones','1','--games-per-seat','1','--evaluation-max-steps','200']
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=300)
 stage=root/'experiment/milestone-001'
 reports={r:json.loads((stage/r/'report.json').read_text()) for r in ('original','tightened')}
 for name,report in reports.items():
  assert report['complete'],name
  assert report['manifest']['config']['require_useful_action']==(name=='tightened'),name
  assert report['manifest']['config']['eat_threshold']==(5 if name=='tightened' else 0),name
  assert report['manifest']['config']['stall_limit']==(8 if name=='tightened' else 0),name
  assert report['manifest']['config']['sacrifice_yields_nothing']==(name=='tightened'),name
 # Both arms must have taken exactly one iteration from the shared seed.
 spec=json.loads((root/'experiment/experiment.json').read_text())
 for arm in ('baseline','tightened'):
  base=root/'experiment'/arm/'2p-r3'
  g=json.loads((base/'latest.json').read_text())['generation']
  state=json.loads((base/'snapshots'/g/'state.json').read_text())
  assert state['iteration']==spec['initial_iteration']+1,(arm,state['iteration'])
  want=(5 if arm=='tightened' else 0)
  assert state['config']['eat_threshold']==want,(arm,state['config'])
  assert state['config']['require_useful_action']==(arm=='tightened'),(arm,state['config'])
  assert state['config']['sacrifice_yields_nothing']==(arm=='tightened'),(arm,state['config'])
 before=json.loads((root/'experiment/progress.json').read_text())
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=300)
 after=json.loads((root/'experiment/progress.json').read_text())
 assert before['completed_rounds']==after['completed_rounds']==1,(before,after)
 # A graceful stop must leave status.json saying so, not frozen mid-arm.
 halted=root/'halted'
 command=[c if c!=str(root/'experiment') else str(halted) for c in command]
 halted.mkdir();(halted/'STOP').touch()
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=300)
 status=json.loads((halted/'status.json').read_text())
 assert status['stage']=='stopped',status
 assert status['reason'] and status['resume'],status
 assert status['stopped_during']['arm'] in ('baseline','tightened'),status
print('Rules ablation: equal updates, per-arm rules, both rule sets, idempotent resume, honest stop passed:',root)
