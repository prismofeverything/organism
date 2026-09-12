"""Exercise graceful mid-game stop and compare resumed/continuous CPU training."""
import json, subprocess, tempfile, time
from pathlib import Path
exe=Path('native/target/release/organism-train').resolve()
root=Path(tempfile.mkdtemp(prefix='organism-resume-'))
flags=['train','--cpu','--players','2','--actors','2','--sims','8','--max-steps','80','--blocks','1','--filters','8','--train-steps','2','--batch-size','8','--eval-every','0','--iters','2']
def command(name):return [str(exe),*flags,'--checkpoint',str(root/name)]
def saved(name):
    base=root/name/'2p'; index=json.loads((base/'latest.json').read_text())
    return json.loads((base/'snapshots'/index['generation']/'state.json').read_text())
log=(root/'log.txt').open('w')
p=subprocess.Popen(command('resume'),stdout=log,stderr=subprocess.STDOUT)
try:
    deadline=time.monotonic()+60
    while time.monotonic()<deadline:
        status=root/'resume/2p/status.json'
        if status.exists() and json.loads(status.read_text()).get('stage')=='self_play':break
        if p.poll() is not None:raise RuntimeError((root/'log.txt').read_text())
        time.sleep(.01)
    else:raise TimeoutError('no self-play status')
    time.sleep(.1)
    (root/'resume/STOP').touch()
    assert p.wait(timeout=30)==0
    stopped=saved('resume')
    assert stopped['iteration']==0 and stopped['episodes']
    assert any(e['samples'] for e in stopped['episodes']), 'stop happened before any decision'
    (root/'resume/STOP').unlink()
    subprocess.run(command('resume'),stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
    subprocess.run(command('continuous'),stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
    a,b=saved('resume'),saved('continuous')
    for state in [a,b]:
        for episode in state['episodes']:
            episode.pop('started')  # wall-clock metadata is intentionally different
            episode['seen'].sort(key=lambda pair: json.dumps(pair,sort_keys=True))
    for key in ['iteration','rng','replay','replay_pos','training_step','episodes']:
        assert a[key]==b[key],key
    assert a['iteration']==2 and len(a['replay'])>0
    print('Mid-game stop/resume preserved exact replay, search targets and RNG:',root)
finally:
    if p.poll() is None:p.terminate();p.wait(timeout=30)
    log.close()
