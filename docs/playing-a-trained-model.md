# Playing a trained model on the website

A trained network can take a seat in a web game as the bot **NEURON**, on a
three-player, four-ring board — the board it was trained on. It needs no GPU.

## How it fits together

```
  web process (JVM)                      move server (Rust, one per web process)
  ────────────────                       ──────────────────────────────────────
  organism.native-bot                    organism-train serve
    position  ──── {"position": …} ────▶   deserialize, search, choose
    choice-for ◀── {"action": 27, …} ───   the action index it picked
```

The network lives in the Rust process because that is where it was trained.
Loading 2.36M parameters takes seconds, so one server is started on the first
move asked of it and kept for the life of the web process. Requests are
serialized over its pipe: one conversation, one answer at a time.

Positions cross as **whole boards**, not as lists of moves. A web game keeps an
undo stack of states and has no move history to replay, so the only thing it can
hand over is the position it is in. `State` in `native/src/game.rs` already
derives `Deserialize`; `organism.native-bot/position` writes exactly that shape.

## Setting it up

One command does the whole deploy — site and bot together:

```sh
./deploy.sh ship
```

It builds the ClojureScript and the uberjar, builds the move server against a
CPU libtorch, publishes the current weights, uploads all of it, restarts the
service once, and then asks the move server on the box for one move so you can
see what a decision costs there.

The box needs no Rust toolchain and no CUDA: the binary and the libraries it
links are built here and shipped, which is how the other native deploys on that
machine work. The libraries are the bulk of the first upload and rsync skips
them thereafter, so later deploys send the jar, the binary and the weights.

Afterwards, **seat NEURON** — it appears in the lobby's player search like OBO.
On any board other than three players on four rings it declines, and the game
falls back to OBO, so nothing hangs.

When training produces a model worth publishing, ship just the bot:

```sh
./deploy.sh model          # rebuild, upload weights, restart
./deploy.sh model-check    # time one move on the box
```

The site keeps serving the old weights until you do — publishing is a decision,
not a side effect of training. The move server loads weights once at startup,
which is why both commands end in a restart.

### Doing it by hand

```sh
bash native/build-serve.sh      # move server, CPU libtorch
bash native/publish-bot.sh 3p   # weights → checkpoints/organism-native/3p/serve.ot
```

`native/build-gpu.sh` builds the trainer instead; it links the CUDA libtorch —
1.6 GB of libraries and a driver to load them — and is not what a web server
wants. The `serve` feature links libtorch and leaves the trainer out.

### Where the settings come from

Three layers, each overriding the last:

1. **Built-in defaults** (`organism.native-bot/base-settings`) — the paths a
   built checkout uses, so the bot works in development with no setup.
2. **`bot/settings.edn`**, beside the app's working directory — what
   `./deploy.sh ship` writes on the box: absolute paths to the shipped binary,
   libraries and weights, plus how hard to think. It is a file rather than the
   service's environment because the deploying user may restart the service and
   nothing else; editing the systemd unit would need a root the deploy
   deliberately does not have.
3. **Environment variables**, for a one-off: `ORGANISM_BOT_SERVE`,
   `ORGANISM_BOT_TORCH`, `ORGANISM_BOT_WEIGHTS`, `ORGANISM_BOT_SIMS`,
   `ORGANISM_BOT_THREADS`.

If the binary or the weights are not where the settings say, NEURON declines the
game and OBO plays instead — a misconfigured deploy costs a weaker opponent, not
a game stuck waiting for a move that never comes.

## Cost

Measured on the position path the bot actually uses — 8x128 network, CPU,
three-player four-ring board, while the machine was also training (load ~9 of
24 cores), so an idle host is faster:

| `ORGANISM_BOT_SIMS` | threads | per decision (median) | per turn (~5.6 decisions) |
| --- | --- | --- | --- |
| 64 | 4 | 0.57 s | 3.2 s |
| 64 | 2 | 0.80 s | 4.5 s |
| 32 | 4 | 0.30 s | 1.7 s |
| 16 | 4 | 0.15 s | 0.8 s |

Cost scales with simulations about linearly, and that is the one knob that
trades thinking time against strength. The default is 64, which is what the
three-player model searched during training; 32 halves the wait for a visibly
snappier bot. One server answers one request at a time, so several games at
once queue behind each other rather than each getting their own.

## The rules the bot plays

The model was trained under three rules that close holes trained agents found
in the game as originally written — no deliberate passing, no eating past five
food, and no food left behind by wiping yourself out. See
`rule-tightening-experiment.md` for what the agents were doing and what closing
it cost.

**The website now plays those rules too**, so the move server runs with them
on. That agreement is not optional: if the server disagreed with the website
about which moves exist, it could not read the website's positions at all — a
human who passed, or ate at five food, would produce a position it rejects.

All three engines enforce them: `organism.game`/`organism.choice` for the
website, `alphazero/games/organism/` for the Python reference, and
`Rules` in `native/src/game.rs` for the trainer. Rust keeps them as flags
because training compares them against the older game; the other two play them
unconditionally. `organism.game/*eat-threshold*` rebinds to `*food-limit*` for
the original eating rule, which is how the tests check both.

Games are about half as long under these rules: a network playing itself on the
website's engine finished in 269 decisions where the same network under the
loose rules took 674.

## What holds the two engines together

The bot reads a position out of one engine and a move out of the other, so any
disagreement about which moves exist — or about what a move index means — is a
wrong move on the website. Two checks hold that agreement in place:

- `tests/check_native_parity.py` — the Rust and Python engines agree on legal
  moves, boards and encodings, and `tests/test_clojure_parity.py` ties Python to
  the live Clojure rules.
- `lein run -m organism.scripts.check-native-bot [games]` — walks whole games,
  and at every decision checks that both engines offer the same set of action
  indices and that the move the bot picks is one the Clojure game is offering.
  It walks randomly rather than letting the network play: a trained network
  answers the same position the same way every time, so a self-play check would
  replay one game however many were asked for.

One structural difference is handled rather than asserted away: funding a growth
is a single choice in Clojure (which growers pay, all at once) and a run of
choices in Rust (one donor space per food). `grow-from-choice` replays that run
against the server and reads the finished allocation off the action it lands on.
