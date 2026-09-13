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
print('Isolated comparison, resume, and manifest mismatch rejection passed:',root)
