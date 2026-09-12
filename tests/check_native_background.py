"""Ensure evaluation progresses alongside self-play and resumes its frozen job."""
import hashlib, json, subprocess, tempfile, time
from pathlib import Path
exe=Path('native/target/release/organism-train').resolve()
root=Path(tempfile.mkdtemp(prefix='organism-background-'))
flags=['train','--cpu','--players','2','--rings-2p','3','--actors','2','--concurrent-games','4','--gpu-batch','2','--sims','4','--max-steps','100','--blocks','1','--filters','8','--train-steps','2','--batch-size','8','--eval-every','1','--eval-games-per-seat','1','--checkpoint',str(root)]
log=(root/'log.txt').open('w')
p=subprocess.Popen([str(exe),*flags,'--forever'],stdout=log,stderr=subprocess.STDOUT)
try:
    deadline=time.monotonic()+60
    while time.monotonic()<deadline:
        pointer=root/'2p-r3/pending-evaluation.json'
        progress=root/'2p-r3/evaluation-progress.json'
        metrics=root/'2p-r3/metrics.jsonl'
        if pointer.exists() and progress.exists() and metrics.exists():
            pr=json.loads(progress.read_text())
            rows=[json.loads(l) for l in metrics.read_text().splitlines()]
            if pr['stage'].startswith('running') and max(pr['choices'])>0 and rows[-1]['iteration']>pr['iteration']:break
        if p.poll() is not None:raise RuntimeError((root/'log.txt').read_text())
        time.sleep(.01)
    else:raise TimeoutError('evaluation did not overlap self-play')
    (root/'STOP').touch();assert p.wait(timeout=30)==0
    pending=json.loads(pointer.read_text())['iteration']
    job=root/f'2p-r3/evaluation-jobs/{pending}'
    state=json.loads((job/'state.json').read_text())
    assert max(state['session']['steps'])>0
    digest=hashlib.sha256((job/'candidate.ot').read_bytes()).hexdigest()
    (root/'STOP').unlink()
    subprocess.run([str(exe),*flags,'--iters','1'],stdout=log,stderr=subprocess.STDOUT,check=True,timeout=60)
    assert hashlib.sha256((job/'candidate.ot').read_bytes()).hexdigest()==digest
    report=json.loads((root/f'2p-r3/evaluations/{pending:06}.json').read_text())
    assert len(report['games'])==2
    print('Self-play advanced while evaluation was pending; frozen evaluation resumed and completed:',root)
finally:
    if p.poll() is None:p.terminate();p.wait(timeout=30)
    log.close()
