// Headless-Chromium profiler for the Future client.
//
// Boots a real browser against http://localhost:11551/future/generate,
// waits for the game to start, drives ~N bot steps, and reports:
//   - console messages emitted from CLJS (RX and RENDER profile lines)
//   - render frequency (renders per second)
//   - per-render duration distribution
//   - long tasks (>50ms) recorded via PerformanceObserver
//
// Run (from repo root, inside `nix develop`):
//   node dev/profile_client.mjs
//
// Requires: chromium available on PATH (see flake.nix devShell).

import { spawn } from 'child_process';
import { setTimeout as sleep } from 'timers/promises';
import http from 'http';
import { EventEmitter } from 'events';

const HOST = 'localhost';
const PORT = 11551;
const RUN_SECONDS = 20;
const DEBUG_PORT = 9222;
const URL = `http://${HOST}:${PORT}/future/generate`;

function checkServer() {
  return new Promise((resolve, reject) => {
    const req = http.request({ host: HOST, port: PORT, path: '/future', timeout: 2000 },
      (res) => { resolve(res.statusCode); res.resume(); });
    req.on('error', reject);
    req.on('timeout', () => { req.destroy(new Error('timeout')); });
    req.end();
  });
}

async function main() {
  console.log(`Checking http://${HOST}:${PORT}/future is up…`);
  try {
    const code = await checkServer();
    console.log(`  → ${code}`);
  } catch (e) {
    console.error(`Server is not reachable: ${e.message}`);
    console.error(`Start it with:  nix develop --command lein run`);
    process.exit(1);
  }

  const exe = process.env.PUPPETEER_EXECUTABLE_PATH || 'chromium';
  const userDataDir = `/tmp/chromium-profile-${process.pid}`;

  console.log(`Launching headless Chromium at ${exe} on :${DEBUG_PORT}…`);
  const chrome = spawn(exe, [
    '--headless=new',
    `--remote-debugging-port=${DEBUG_PORT}`,
    `--user-data-dir=${userDataDir}`,
    '--no-sandbox',
    '--disable-gpu',
    '--window-size=1400,900',
    'about:blank'
  ], { stdio: ['ignore', 'pipe', 'pipe'] });

  chrome.stderr.on('data', (d) => {
    const line = d.toString();
    // suppress noise; keep obvious errors
    if (/ERROR|FATAL/i.test(line)) process.stderr.write(`[chrome] ${line}`);
  });

  // Wait for CDP endpoint
  let versionInfo = null;
  for (let i = 0; i < 40; i++) {
    try {
      versionInfo = await new Promise((res, rej) => {
        const req = http.request({ host: 'localhost', port: DEBUG_PORT, path: '/json/version' }, (r) => {
          let body = ''; r.on('data', c => body += c); r.on('end', () => res(JSON.parse(body)));
        });
        req.on('error', rej); req.end();
      });
      break;
    } catch (e) { await sleep(200); }
  }
  if (!versionInfo) { console.error('CDP did not come up'); chrome.kill(); process.exit(1); }
  console.log(`  → CDP up: ${versionInfo.Browser}`);

  const wsUrl = versionInfo.webSocketDebuggerUrl;
  const { default: WebSocket } = await import('ws').catch(() => {
    console.error('ws module missing — install with:  npm install ws');
    chrome.kill(); process.exit(2);
  });

  const events = new EventEmitter();
  const ws = new WebSocket(wsUrl);
  await new Promise((res, rej) => { ws.once('open', res); ws.once('error', rej); });

  let msgId = 0;
  const pending = new Map();
  ws.on('message', (raw) => {
    const m = JSON.parse(raw.toString());
    if (m.id != null && pending.has(m.id)) {
      const { resolve, reject } = pending.get(m.id);
      pending.delete(m.id);
      if (m.error) reject(new Error(m.error.message || JSON.stringify(m.error)));
      else resolve(m.result);
    } else if (m.method) {
      events.emit(m.method, m.params);
    }
  });

  function send(method, params) {
    const id = ++msgId;
    return new Promise((resolve, reject) => {
      pending.set(id, { resolve, reject });
      ws.send(JSON.stringify({ id, method, params }));
    });
  }

  // Attach to the first target
  const { targetInfos } = await send('Target.getTargets');
  const target = targetInfos.find((t) => t.type === 'page');
  const { sessionId } = await send('Target.attachToTarget', { targetId: target.targetId, flatten: true });

  function sessionSend(method, params) {
    const id = ++msgId;
    return new Promise((resolve, reject) => {
      pending.set(id, { resolve, reject });
      ws.send(JSON.stringify({ id, method, params, sessionId }));
    });
  }

  await sessionSend('Page.enable');
  await sessionSend('Runtime.enable');
  await sessionSend('Log.enable');
  await sessionSend('Performance.enable');
  await sessionSend('Profiler.enable');
  await sessionSend('Profiler.setSamplingInterval', { interval: 200 });

  const consoleLines = [];
  const errors = [];
  events.on('Runtime.consoleAPICalled', (p) => {
    const text = p.args.map(a => a.value != null ? a.value : (a.description || a.type)).join(' ');
    consoleLines.push({ t: Date.now(), level: p.type, text });
  });
  events.on('Runtime.exceptionThrown', (p) => {
    errors.push(JSON.stringify(p.exceptionDetails).slice(0, 300));
  });

  console.log(`Navigating to ${URL}…`);
  await sessionSend('Page.navigate', { url: URL });

  // wait for load event via polling — Page.loadEventFired is a "method"
  // that arrives outside the message ID protocol
  await new Promise((res) => {
    const to = setTimeout(res, 5000);
    events.once('Page.loadEventFired', () => { clearTimeout(to); res(); });
  });
  console.log('Page loaded. Waiting for generate view to boot…');
  // give shadow-cljs a moment to init + gen-new!
  await sleep(1500);

  // Grab initial metrics
  const metrics0 = await sessionSend('Performance.getMetrics');
  const t0 = Date.now();

  console.log(`Recording for ${RUN_SECONDS}s… (bots auto-run at ~80ms in generate view)`);

  // Kick off auto-play by clicking the "Auto" button. Simulate via JS.
  await sessionSend('Runtime.evaluate', {
    expression: `
      (function() {
        const btn = Array.from(document.querySelectorAll('button')).find(b => /^Auto$/.test(b.textContent));
        if (btn) btn.click();
        return btn ? 'clicked' : 'no-auto-button';
      })()
    `,
    returnByValue: true
  }).then(r => console.log('  Auto:', r?.result?.value));

  await sessionSend('Profiler.start');
  await sleep(RUN_SECONDS * 1000);
  const { profile } = await sessionSend('Profiler.stop');

  const metrics1 = await sessionSend('Performance.getMetrics');
  const t1 = Date.now();

  // Stop autoplay to prevent noise
  await sessionSend('Runtime.evaluate', {
    expression: `
      (function() {
        const btn = Array.from(document.querySelectorAll('button')).find(b => /^Stop$/.test(b.textContent));
        if (btn) btn.click();
      })()
    `
  });

  // Analysis
  const elapsedS = (t1 - t0) / 1000;
  const metricsMap = (arr) => Object.fromEntries(arr.metrics.map(m => [m.name, m.value]));
  const m0 = metricsMap(metrics0), m1 = metricsMap(metrics1);

  const rxLines = consoleLines.filter(l => /^RX\[/.test(l.text));
  const renderLines = consoleLines.filter(l => /^RENDER#/.test(l.text));

  const parseNums = (line, re) => {
    const m = line.text.match(re); return m ? parseFloat(m[1]) : null;
  };
  const times = (lines, re) => lines.map(l => parseNums(l, re)).filter(x => x != null);
  const stats = (xs) => {
    if (!xs.length) return { n: 0 };
    const s = [...xs].sort((a,b) => a-b);
    const sum = xs.reduce((a,b) => a+b, 0);
    return { n: xs.length,
             sum: +sum.toFixed(1),
             avg: +(sum / xs.length).toFixed(2),
             p50: +s[Math.floor(s.length * 0.5)].toFixed(2),
             p95: +s[Math.floor(s.length * 0.95)].toFixed(2),
             max: +s[s.length - 1].toFixed(2) };
  };

  const rxParse = stats(times(rxLines, /parse=([\d.]+)/));
  const rxLA    = stats(times(rxLines, /la=([\d.]+)/));
  const rxTotal = stats(times(rxLines, /total=([\d.]+)/));
  const rxSize  = stats(times(rxLines, /^RX\[(\d+)B/));
  const renderDerive = stats(times(renderLines, /derive=([\d.]+)/));

  console.log('\n══════════════════════════════════════════════════════════════');
  console.log(`Ran for ${elapsedS.toFixed(1)}s`);
  console.log('──────────────────────────────────────────────────────────────');
  console.log(`WS messages received:  ${rxLines.length}   (${(rxLines.length/elapsedS).toFixed(1)}/s)`);
  console.log(`Renders fired:         ${renderLines.length}   (${(renderLines.length/elapsedS).toFixed(1)}/s)`);
  console.log(`Console errors:        ${errors.length}`);
  console.log('──────────────────────────────────────────────────────────────');
  console.log('WS message wire size (bytes):');
  console.log(' ', rxSize);
  console.log('WS receive breakdown (ms per msg):');
  console.log('  parse            ', rxParse);
  console.log('  legal-actions    ', rxLA);
  console.log('  total (parse+la) ', rxTotal);
  console.log('board-svg derive-clickable (ms per render):');
  console.log(' ', renderDerive);
  console.log('──────────────────────────────────────────────────────────────');
  console.log('Chromium Performance.getMetrics delta:');
  for (const key of [
    'TaskDuration', 'ScriptDuration', 'V8CompileDuration',
    'RecalcStyleDuration', 'LayoutDuration', 'JSHeapUsedSize', 'JSHeapTotalSize'
  ]) {
    if (m0[key] != null && m1[key] != null) {
      const d = m1[key] - m0[key];
      if (key.endsWith('Duration')) console.log(`  ${key.padEnd(24)} +${d.toFixed(2)}s`);
      else                          console.log(`  ${key.padEnd(24)} ${m1[key].toFixed(0)} bytes (Δ ${(d/1024).toFixed(1)} KB)`);
    }
  }
  if (errors.length) {
    console.log('──────────────────────────────────────────────────────────────');
    console.log('First 3 exceptions:');
    for (const e of errors.slice(0, 3)) console.log('  ', e);
  }
  console.log('──────────────────────────────────────────────────────────────');
  console.log('CPU profile — hottest self-time functions:');
  // Compute per-node self-time from CPU profile
  const nodes = profile.nodes;
  const samples = profile.samples || [];
  const timeDeltas = profile.timeDeltas || [];
  const selfTime = new Map();  // nodeId → μs
  for (let i = 0; i < samples.length; i++) {
    const id = samples[i];
    const dt = timeDeltas[i] || 0;
    selfTime.set(id, (selfTime.get(id) || 0) + dt);
  }
  const totalUs = [...selfTime.values()].reduce((a, b) => a + b, 0);
  const byId = new Map(nodes.map(n => [n.id, n]));
  // Build a rough parent map to help name anonymous nodes
  const parentOf = new Map();
  for (const n of nodes) for (const c of n.children || []) parentOf.set(c, n.id);
  function nodeName(n) {
    const fn = n.callFrame || {};
    const raw = fn.functionName || '(anonymous)';
    const url = (fn.url || '').split('/').pop();
    const line = fn.lineNumber != null ? `:${fn.lineNumber}` : '';
    return `${raw}  [${url}${line}]`;
  }
  const ranked = [...selfTime.entries()]
    .map(([id, us]) => ({ id, us, node: byId.get(id) }))
    .filter(x => x.node)
    .sort((a, b) => b.us - a.us);
  const top = ranked.slice(0, 25);
  for (const { us, node } of top) {
    const pct = ((us / totalUs) * 100).toFixed(1);
    console.log(`  ${(us/1000).toFixed(1).padStart(7)}ms  ${pct.padStart(5)}%  ${nodeName(node)}`);
  }
  console.log(`  (total sampled: ${(totalUs/1000).toFixed(0)}ms across ${samples.length} samples)`);
  console.log('══════════════════════════════════════════════════════════════');

  ws.close();
  chrome.kill();
  process.exit(0);
}

main().catch((e) => { console.error(e); process.exit(3); });
