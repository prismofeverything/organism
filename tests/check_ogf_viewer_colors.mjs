// Requires the dashboard on localhost:8765 and Firefox WebDriver BiDi on :9229.
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

const health=JSON.parse(await run("JSON.stringify({error:document.getElementById('error').textContent,paths:document.querySelectorAll('#board path').length,labels:document.querySelectorAll('#board text').length,step:game?.frames.at(-1)?.step})"));
if(health.error||!health.paths||!health.labels)throw Error('Live viewer failed: '+JSON.stringify(health));
console.log('Live viewer renders:',health);
const original=JSON.parse(await run('JSON.stringify(game)'));
for(const css of ['hsl(45.158,42.251%,79.731%)','rebeccapurple','#849cd5','rgba(120, 50, 200, 0.5)']){
 const fixture=structuredClone(original);
 fixture.name='Color regression '+css;fixture.id=fixture.name;
 fixture.board['ring-colors']=fixture.board['ring-colors'].map(()=>css);
 fixture.frames=[{turn:0,round:0,player:fixture.players[0],elements:[[fixture.players[0],'eat',fixture.board.spaces[1],2]],food:{},captures:{}}];
 await run("document.getElementById('back').click()");
 await run(`load(${JSON.stringify(fixture)})`);
 if(!await run("document.querySelectorAll('#board path').length>0&&document.querySelectorAll('#board text').length>0"))throw Error('Color did not render: '+css);
 const exported=JSON.parse(await run("(async()=>JSON.stringify(await(await fetch(document.getElementById('download').href)).json()))()"));
 if(exported.board['ring-colors'].some(c=>c!==css))throw Error('Original saved color changed: '+css);
 console.log('Color renders and round-trips unchanged:',css);
}
await send('browsingContext.close',{context});
} finally {try{await send('session.end',{})}catch{}ws.close()}
