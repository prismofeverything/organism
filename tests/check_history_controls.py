"""The history buttons must not move while you step through a game.

Reported twice and guessed at twice, wrongly, before anybody measured it. The
Next button was landing in four different places over twelve steps, a 119px
spread, because the action-controls block above it is absent at one phase and
present at others and swings from 73px to 212px with the phase. Every step moved
the button out from under the pointer.

This loads the real page in a real browser and records where the button actually
is, rather than reasoning about the CSS. When it fails it also prints the height
of every block above the button, per step, so the culprit names itself instead of
being inferred.

    lein run -p 3111                       # the site, against the local database
    geckodriver --port 4444                # in another shell
    python3 tests/check_history_controls.py [--game KEY] [--steps N]
"""
import json, time, urllib.request

import argparse, sys, urllib.parse

parse = argparse.ArgumentParser()
parse.add_argument("--driver", default="http://127.0.0.1:4444")
parse.add_argument("--site", default="http://127.0.0.1:3111")
parse.add_argument("--game", default="2p Testing!")
parse.add_argument("--steps", type=int, default=12)
options = parse.parse_args()

DRIVER = options.driver
GAME = f"{options.site}/organism/play/{urllib.parse.quote(options.game)}"

def call(method, path, body=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(f"{DRIVER}{path}", data=data, method=method,
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=60) as r:
        return json.loads(r.read() or b"{}")

session = call("POST", "/session", {"capabilities": {"alwaysMatch": {
    "browserName": "firefox",
    "moz:firefoxOptions": {"args": ["-headless", "-width", "1400", "-height", "1000"]}}}})
sid = session["value"]["sessionId"]

def script(js, args=()):
    return call("POST", f"/session/{sid}/execute/sync", {"script": js, "args": list(args)})["value"]

try:
    call("POST", f"/session/{sid}/url", {"url": GAME})
    time.sleep(6)

    # Find the history buttons by their labels, wherever they are in the DOM.
    # Found by what the button is FOR, not what it says: the labels are icons
    # now, and keying on the glyph would break again the next time they change.
    find = """
      const wanted = {'First position':'First','Back one':'Back',
                      'Forward one':'Next','Latest position':'Latest'};
      const out = {};
      for (const b of document.querySelectorAll('button')) {
        const key = wanted[b.getAttribute('aria-label') || ''];
        if (key && !(key in out)) {
          const r = b.getBoundingClientRect();
          out[key] = {top: Math.round(r.top + window.scrollY), left: Math.round(r.left), h: Math.round(r.height)};
        }
      }
      return JSON.stringify(out);
    """
    first = json.loads(script(find))
    if "Next" not in first:
        print("could not find the history buttons; page may not have rendered")
        print("buttons seen:", script(
            "return JSON.stringify([...document.querySelectorAll('button')].map(b=>b.textContent.trim()).slice(0,25))"))
        raise SystemExit(1)

    # The very first thing a person does: open the game and press First. The
    # original check stepped forward from the start and never measured this
    # jump, which is how an 80px shift shipped — the winner line in the
    # scoreboard exists at the last position and not at the first.
    landing = json.loads(script(find))
    script("""
      for (const b of document.querySelectorAll('button')) {
        if (b.getAttribute('aria-label') === 'First position' && !b.disabled) { b.click(); return true; }
      }
      return false;""")
    time.sleep(1.5)
    after_first = json.loads(script(find))
    if landing.get("Next") and after_first.get("Next"):
        shift = after_first["Next"]["top"] - landing["Next"]["top"]
        print(f"  opening the game and pressing First moves Next {shift:+d}px")
        if shift:
            print("  THE NAVIGATION MOVES on the very first click.")
            sys.exit(1)
        print()

    # Start at the beginning, or Next is disabled from the outset.
    script("""
      for (const b of document.querySelectorAll('button')) {
        if (b.getAttribute('aria-label') === 'First position' && !b.disabled) { b.click(); return true; }
      }
      return false;""")
    time.sleep(1.5)

    print("stepping through history, recording where Next actually sits:\n")
    print(f"  {'step':>4}  {'Next.top':>9}{'Next.left':>10}   moved")
    positions = []
    shapes = []
    for step in range(options.steps):
        boxes = json.loads(script(find))
        nxt = boxes.get("Next")
        if not nxt:
            print(f"  {step:>4}  Next button vanished"); break
        moved = "" if not positions else (
            "—" if (nxt["top"], nxt["left"]) == positions[-1] else
            f"<<< moved {nxt['top']-positions[-1][0]:+d}px vertically")
        print(f"  {step:>4}  {nxt['top']:>9}{nxt['left']:>10}   {moved}")
        positions.append((nxt["top"], nxt["left"]))
        shape = script("""
          const next = [...document.querySelectorAll('button')].find(b=>b.getAttribute('aria-label')==='Forward one');
          const out = []; let node = next;
          while (node && node !== document.body) {
            let sib = node.previousElementSibling;
            while (sib) {
              const r = sib.getBoundingClientRect();
              if (r.height > 0) out.push(Math.round(r.height) + ':' + (sib.textContent||'').trim().slice(0,22).replace(/\\s+/g,' '));
              sib = sib.previousElementSibling;
            }
            node = node.parentElement;
          }
          return JSON.stringify(out);
        """)
        shapes.append(json.loads(shape))
        clicked = script("""
          for (const b of document.querySelectorAll('button')) {
            if (b.getAttribute('aria-label') === 'Forward one' && !b.disabled) { b.click(); return true; }
          }
          return false;""")
        if not clicked:
            print("   (Next disabled — end of history)"); break
        time.sleep(0.7)

    # Compare by position in the stack, not by text -- the text changes every
    # step, which made every block look like a different block.
    print("\n  blocks above Next, by position in the stack:")
    width = max(len(sh) for sh in shapes)
    for i in range(width):
        heights, texts = [], []
        for sh in shapes:
            if i < len(sh):
                h, _, t = sh[i].partition(':')
                heights.append(int(h)); texts.append(t)
            else:
                heights.append(None)
        seen = sorted({h for h in heights if h is not None})
        missing = sum(1 for h in heights if h is None)
        label = texts[0][:30] if texts else ""
        if len(seen) > 1 or missing:
            note = f"{min(seen)}..{max(seen)}px" if seen else "-"
            extra = f"  (absent in {missing} steps)" if missing else ""
            print(f"    VARIES  {note:<12}{extra:<22} {label!r}")
        else:
            print(f"    steady  {seen[0]:>4}px{'':<28} {label!r}")

    tops = [p[0] for p in positions]
    print(f"\n  distinct vertical positions: {len(set(tops))} over {len(tops)} steps")
    print(f"  range: {min(tops)}..{max(tops)}  (spread {max(tops)-min(tops)}px)")
    if len(set(tops)) > 1:
        print("\n  THE BUTTON MOVES. Something above it is changing height.")
        # What is above it, and how tall is each thing?
        above = script("""
          const next = [...document.querySelectorAll('button')].find(b=>b.getAttribute('aria-label')==='Forward one');
          const out = []; let node = next;
          while (node && node !== document.body) {
            let sib = node.previousElementSibling;
            while (sib) {
              const r = sib.getBoundingClientRect();
              if (r.height > 0) out.push({tag: sib.tagName, cls: sib.className||'', h: Math.round(r.height),
                                          text: (sib.textContent||'').trim().slice(0,40)});
              sib = sib.previousElementSibling;
            }
            node = node.parentElement;
          }
          return JSON.stringify(out.slice(0, 12));
        """)
        print("\n  siblings above Next, with heights:")
        for s in json.loads(above):
            print(f"    {s['h']:>5}px  {s['tag']:<6} {s['text']!r}")
    else:
        print("\n  the button holds still.")
        sys.exit(0)
    sys.exit(1)
finally:
    call("DELETE", f"/session/{sid}")
