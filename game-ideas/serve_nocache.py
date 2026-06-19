#!/usr/bin/env python3
"""Static server for the Chroma mock that disables caching, so every phone
refresh fetches the latest file (no stale-cache surprises during iteration).
Serves /home/m/organism/game-ideas on 0.0.0.0:8770 (LAN + Tailscale reachable)."""
import http.server, socketserver, os, json, time, re

BASE = "/home/m/organism/game-ideas"
LOGDIR = os.path.join(BASE, "chroma-logs")
os.chdir(BASE)
os.makedirs(LOGDIR, exist_ok=True)
PORT = 8770

class NoCacheHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        self.send_header("Pragma", "no-cache")
        self.send_header("Expires", "0")
        super().end_headers()

    # Mock POSTs each finished game here so every play is analyzable on disk.
    def do_POST(self):
        if self.path.rstrip("/") != "/save-log":
            self.send_error(404); return
        try:
            n = int(self.headers.get("Content-Length", 0))
            rec = json.loads(self.rfile.read(n).decode("utf-8"))
            stamp = time.strftime("%Y%m%dT%H%M%SZ", time.gmtime())
            seed = re.sub(r"[^0-9a-zA-Z_-]", "", str(rec.get("seed", "x")))[:16]
            fn = f"game-{stamp}-{seed}.json"
            with open(os.path.join(LOGDIR, fn), "w") as f:
                json.dump(rec, f, indent=1)
            self.send_response(200)
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write(json.dumps({"saved": fn}).encode())
        except Exception as e:
            self.send_error(500, str(e))

socketserver.TCPServer.allow_reuse_address = True
with socketserver.TCPServer(("0.0.0.0", PORT), NoCacheHandler) as httpd:
    print(f"serving {os.getcwd()} on :{PORT} with no-cache headers")
    httpd.serve_forever()
