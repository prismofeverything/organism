// Requires local dashboard :8765 and Firefox WebDriver BiDi :9229.
import fs from 'node:fs';
const ws = new WebSocket('ws://127.0.0.1:9229/session');
await new Promise((r,j)=>{ws.onopen=r;ws.onerror=j});
let id=0;const pending=new Map();ws.onmessage=e=>{const m=JSON.parse(e.data);if(pending.has(m.id)){const [r,j]=pending.get(m.id);pending.delete(m.id);m.type==='error'?j(Error(JSON.stringify(m))):r(m.result)}};
function send(method,params){console.log('Checking',method);return new Promise((r,j)=>{const n=++id;pending.set(n,[r,j]);const timeout=setTimeout(()=>{j(Error('Timed out: '+method));ws.close()},20000);timeout.unref();ws.send(JSON.stringify({id:n,method,params}))})}
try {
await send('session.new',{capabilities:{alwaysMatch:{acceptInsecureCerts:true}}});
const {context}=await send('browsingContext.create',{type:'tab'});
await send('browsingContext.navigate',{context,url:'http://127.0.0.1:8765',wait:'complete'});
const run=async expression=>{const r=await send('script.evaluate',{expression,target:{context},awaitPromise:true});if(r.type==='exception')throw Error(JSON.stringify(r));return r.result.value};
await new Promise(r=>setTimeout(r,3500));

const before=JSON.parse(await run("JSON.stringify({error:document.getElementById('error').textContent,selected,step:game?.frames.at(-1)?.step,plots:document.querySelectorAll('#cards .outcomes').length,scoreBars:document.querySelectorAll('#scores .track').length,history:document.querySelectorAll('#game-history svg').length})"));
if(before.error||before.plots<1||before.scoreBars<4||!before.history)throw Error('Visual dashboard did not render: '+JSON.stringify(before));
console.log('Visual dashboard',before);
await new Promise(r=>setTimeout(r,2000));
const after=JSON.parse(await run("JSON.stringify({step:game?.frames.at(-1)?.step,id:game?.id,error:document.getElementById('error').textContent})"));
if(after.error)throw Error(after.error);
await run("document.getElementById('back').click()");
const frozen=await run('JSON.stringify({index,id:game.id})');
await new Promise(r=>setTimeout(r,800));
if(frozen!==await run('JSON.stringify({index,id:game.id})'))throw Error('Scrubbing did not pause');
await run("document.getElementById('history-metric').click()");
if(!await run("document.getElementById('history-label').textContent.startsWith('Power')"))throw Error('History toggle failed');
if(!await run("document.querySelector('#game-history [data-power-target=\"5\"]')!==null"))throw Error('Missing power victory threshold');
const powerScale=JSON.parse(await run("JSON.stringify((()=>{const f=structuredClone(game.frames[index]);f.captures=Object.fromEntries(game.players.map((p,i)=>[p,i?1:2]));drawScores(f);const bars=[...document.querySelectorAll('#scores .score-bars')];const result=bars.map(b=>({width:b.querySelectorAll('.track>span')[1].style.width,value:b.querySelectorAll('.value')[1].textContent}));draw();return result})())"));
if(powerScale[0].width!=='40%'||powerScale[0].value!=='2 / 5'||powerScale[1].width!=='20%')throw Error('Power bars are not scaled to the winning threshold');
await run("document.getElementById('history-metric').click()");
const counts=JSON.parse(await run("JSON.stringify((()=>{const svg=outcomeChart([{iteration:1,games:[{termination:'win'},{termination:'repetition'},{termination:'max_steps'},{termination:'unexpected'}]}]);return {heights:[...svg.querySelectorAll('rect')].map(r=>Number(r.getAttribute('height'))),label:svg.getAttribute('aria-label')}})())"));
if(counts.heights.length!==4||counts.heights.some(h=>h!==13.5)||!counts.label.includes('1 victories'))throw Error('Outcome chart counts incorrect');
await run("document.querySelector('#cards .diagnostic').open=true");
await new Promise(r=>setTimeout(r,3300));
if(!await run("document.querySelector('#cards .diagnostic').open"))throw Error('Polling closes diagnostics');
await run("document.querySelector('#cards .diagnostic').open=false");
await run('window.scrollTo(0,0)');
let shot=await send('browsingContext.captureScreenshot',{context,origin:'viewport'});
fs.writeFileSync('checkpoints/visual-dashboard-preview.png',Buffer.from(shot.data,'base64'));
await send('browsingContext.setViewport',{context,viewport:{width:390,height:844},devicePixelRatio:1});
await new Promise(r=>setTimeout(r,300));
if(!await run('document.documentElement.scrollWidth<=window.innerWidth+1'))throw Error('Mobile layout overflows');
shot=await send('browsingContext.captureScreenshot',{context,origin:'viewport'});
fs.writeFileSync('checkpoints/visual-dashboard-mobile.png',Buffer.from(shot.data,'base64'));
console.log('Charts, playback pause, history toggle, persistent diagnostics and mobile layout passed');
await send('browsingContext.close',{context});
} finally {try{await send('session.end',{})}catch{}ws.close()}
