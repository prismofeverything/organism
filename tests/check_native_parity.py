"""Differential action/position/encoding check against the native Rust executable."""
import json, random, subprocess, sys
from pathlib import Path
import numpy as np
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from alphazero.games.organism.game import OrganismGame
from test_clojure_parity import snapshot

def main():
    rng=random.Random(12)
    child=subprocess.Popen(['native/target/release/organism-train'],stdin=subprocess.PIPE,stdout=subprocess.PIPE,text=True)
    checks=0
    try:
        for players in (2,3):
            for run in range(3):
                game=OrganismGame(players,num_rings=4,remove_notches=False)
                state=game.initial_state();path=[]
                for step in range(1200):
                    legal=game.legal_actions(state)
                    if step<50 or step%10==0 or not legal:
                        child.stdin.write(json.dumps({'players':players,'actions':path})+'\n');child.stdin.flush()
                        line=child.stdout.readline()
                        if not line:raise RuntimeError('Rust oracle exited')
                        actual=json.loads(line)
                        context=f'{players}p run={run} step={step} path={path}'
                        assert actual['snapshot']==snapshot(state),context+'\nsnapshot mismatch\n'+str(actual['snapshot'])+'\n'+str(snapshot(state))
                        assert sorted(actual['legal'])==sorted(legal),context+'\nlegal mismatch '+str(actual['legal'])+' vs '+str(list(legal))
                        assert actual['round']==state['state']['round'],context+' round'
                        expected=game.encode_state(state,state['state']['player_turn']['player']).reshape(-1)
                        np.testing.assert_allclose(actual['encoding'],expected,rtol=1e-6,atol=1e-7,err_msg=context)
                        for action,s in actual['children']:
                            assert s==snapshot(legal[action]),context+f' child {action}'
                        checks+=1
                    if not legal:break
                    action=rng.choice(sorted(legal));path.append(action);state=legal[action]
                print(f'{players}p run {run}: {len(path)} choices, winner={state["state"]["winner"]}',flush=True)
        print(f'Native/Python parity passed: {checks} positions plus all child boards, legal choices and encodings.')
    finally:
        child.stdin.close();child.wait(timeout=10)
if __name__=='__main__':main()
