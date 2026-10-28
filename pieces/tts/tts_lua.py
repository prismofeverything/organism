#!/usr/bin/env python3
"""Run Lua in a running Tabletop Simulator, through its external editor API,
and print what comes back: prints, errors, and the chunk's return value.

TTS listens on localhost:39999 and answers on localhost:39998, so this runs on
the machine TTS runs on:

    ssh host python3 tts_lua.py < chunk.lua            # run Lua, print its prints
    ssh host python3 tts_lua.py --push-save SAVE.json  # load SAVE's Global Lua + UI
                                                       # into the running game (Save & Play)

Standard library only: it is piped to machines with nothing installed.
"""
import json, socket, sys, threading, time

WAIT = float(__import__("os").environ.get("TTS_WAIT", "6"))  # seconds to collect answers after sending


def listen(messages, ready):
    srv = socket.socket()
    srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    srv.bind(("127.0.0.1", 39998))
    srv.listen(8)
    srv.settimeout(0.2)
    ready.set()
    end = time.time() + WAIT
    while time.time() < end:
        try:
            conn, _ = srv.accept()
        except socket.timeout:
            continue
        data = b""
        conn.settimeout(1)
        try:
            while True:
                chunk = conn.recv(65536)
                if not chunk:
                    break
                data += chunk
        except socket.timeout:
            pass
        conn.close()
        try:
            messages.append(json.loads(data.decode("utf-8", "replace")))
        except ValueError:
            messages.append({"raw": data[:500].decode("utf-8", "replace")})


def send(payload):
    s = socket.create_connection(("127.0.0.1", 39999), timeout=5)
    s.sendall(json.dumps(payload).encode())
    s.close()


def main():
    args = sys.argv[1:]
    messages, ready = [], threading.Event()
    t = threading.Thread(target=listen, args=(messages, ready))
    t.start()
    ready.wait()
    if args and args[0] == "--push-save":
        save = json.load(open(args[1]))
        send({"messageID": 1, "scriptStates": [
            {"name": "Global", "guid": "-1", "script": save["LuaScript"], "ui": save["XmlUI"]}]})
    else:
        send({"messageID": 3, "guid": "-1", "script": " ".join(args) or sys.stdin.read()})
    t.join()
    for m in messages:
        kind = m.get("messageID")
        if kind == 2:
            print(m["message"])
        elif kind == 3:
            print("ERROR", m.get("errorMessagePrefix", ""), m.get("error", ""))
        elif kind == 5:
            print("=>", m.get("returnValue"))
        elif kind not in (0, 1):
            print(json.dumps(m)[:300])


if __name__ == "__main__":
    main()
