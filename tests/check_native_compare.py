"""Exercise isolated comparison persistence and immutable manifest checking."""
import json, os, subprocess, tempfile
from pathlib import Path
root=Path(tempfile.mkdtemp(prefix='organism-compare-test-'))
exe=Path('native/target/release/organism-train').resolve()
with (root/'log').open('w') as log:
 subprocess.run([str(exe),'train','--cpu','--players','2','--rings-2p','3','--actors','2','--concurrent-games','2','--sims','2','--max-steps','20','--blocks','1','--filters','8','--train-steps','1','--eval-every','0','--checkpoint',str(root/'model')],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
 model=root/'model/2p-r3';g=json.loads((model/'latest.json').read_text())['generation']
 weights=model/'snapshots'/g/'model.ot'
 spec={'config':json.loads((model/'config.json').read_text()),'candidate':{'weights':str(weights),'identity':'same-a'},'opponent':{'weights':str(weights),'identity':'same-b'},'simulations':2,'games_per_seat':1,'seed':17,'cutoff_value':'mask'}
 manifest=root/'manifest.json';manifest.write_text(json.dumps(spec))
 command=[str(exe),'compare',str(manifest),'--cpu']
 (root/'STOP').touch()
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
 assert not json.loads((root/'report.json').read_text())['complete']
 (root/'STOP').unlink()
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
 report=json.loads((root/'report.json').read_text());assert report['complete'] and len(report['games'])==2
 subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
 assert report==json.loads((root/'report.json').read_text())
 spec['seed']=18;manifest.write_text(json.dumps(spec))
 assert subprocess.run(command,stdout=log,stderr=subprocess.STDOUT,timeout=60).returncode!=0
# Repeat with a three-player board; each candidate seat faces two opponent copies.
with (root/'log').open('a') as log:
 subprocess.run([str(exe),'train','--cpu','--players','3','--actors','2','--sims','2','--max-steps','20','--blocks','1','--filters','8','--train-steps','1','--eval-every','0','--checkpoint',str(root/'three')],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
 model=root/'three/3p';g=json.loads((model/'latest.json').read_text())['generation']
 weights=model/'snapshots'/g/'model.ot';out=root/'three-eval';out.mkdir()
 spec['config']=json.loads((model/'config.json').read_text())
 spec['candidate']['weights']=spec['opponent']['weights']=str(weights)
 (out/'manifest.json').write_text(json.dumps(spec))
 subprocess.run([str(exe),'compare',str(out/'manifest.json'),'--cpu'],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
 report=json.loads((out/'report.json').read_text());assert report['complete'] and len(report['games'])==3
 for game in report['games']:
  assert game['seat_models'][game['seat']]==0
  assert sorted(game['seat_models'])==[0,1,1]
print('2p/3p isolated comparison, seat balance, resume, and manifest mismatch rejection passed:',root)
