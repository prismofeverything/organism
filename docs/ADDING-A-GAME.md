# Adding a game

This is the shape every game on the site now has, written down after doing it
once deliberately (UNIVERSE, `/universe`) with the intent of not having to
rediscover it a fourth time. ORGANISM is the oldest and most complete
implementation and is worth reading, but it predates most of the shared pieces
below — **start from `universe` or `future`, not from `organism`**, and reach
for the shared helper before writing the thing yourself.

## The eight files

For a game called `<game>`:

| file | what goes in it |
|---|---|
| `src/cljc/<game>/game.cljc` | the rules. Pure, no I/O, no atoms. |
| `src/clj/organism/routes/<game>.clj` | HTTP routes and page handlers |
| `src/clj/organism/routes/<game>_ws.clj` | the `games` atom, WebSocket lifecycle, bots |
| `src/clj/organism/persist_<game>.clj` | storage |
| `src/cljs/<game>/play.cljs` | every view, dispatched off JS globals |
| `resources/html/<game>/*.html` | thin shells that set those globals |
| `test/clj/<game>/*_test.clj` | tests against the cljc rules |
| `shadow-cljs.edn` + `handler.clj` | wiring |

`.cljc` for the rules is not decoration. The server is the authority and the
client wants the same answers — which actions are legal, what a hand is worth —
and the only way those two agree forever is for there to be one copy. Keep the
rules free of host interop (no `.indexOf`, no `java.util.*`): it compiles for
the browser too, and that failure shows up late.

## What you should not write again

- **`shared/create-game!`** (`routes/shared.clj`) — the POST handler. Give it
  `:games-atom`, `:make-state`, optionally `:max-players` / `:persist!` /
  `:after!`. It validates, stores the standard game record, persists and
  replies `{:play-key …}`.
- **`shared/require-auth`** — route middleware, redirects to login and back.
- **`shared/delete-game!` / `keep-game!`** — the mark-and-grace deletion flow.
- **`components/create-lobby`** (`cljs/organism/components.cljs`) — the whole
  new-game form: player slots, bot autocomplete, validation, POST, redirect.
  It is themed by argument, so a game supplies colors, not markup.
- **`organism.game-ws`** — transit, `send!`, and the channel registry over your
  `games` atom. Your `*_ws.clj` supplies only the lifecycle.
- **`organism.bots`** — `register-bot!` / `get-agent-step` / `bot?`. Registering
  is what makes a bot appear in the lobby's autocomplete, and it handles the
  auto-suffixed instance names (`ORACLE-A`, `ORACLE-B`) for you.
- **`persist/load-player-games db player "<game>"`** — the games list. Write
  `player-games-<name>` rows in the shape the other games write and the site's
  lists, stats and ratings pick your game up without knowing what it is.

## Games that hide information

Every game here until UNIVERSE broadcast one identical state to every channel,
which is fine for a board everyone can see. A hand of cards is not, and the
plain `:channels` set cannot say who is on the other end of a socket. So:

- `gws/watch!` registers `channel → player` alongside the channel.
- `gws/send-views!` takes that map and a function, and builds a **separate
  message per watcher**.
- The rules namespace supplies the redaction — `view [state player]` — and it
  belongs there, next to the rules, not in the handler.

Redact on the way out of the server. Hole cards the browser never receives are
hole cards it cannot be tricked into showing, and "the client doesn't render
it" is not a secret. Strip the undealt deck too: it gives away the whole hand.

## Games that need a clock

The site is otherwise asynchronous — a turn waits as long as it likes. Poker is
played with other people sitting there, so UNIVERSE keeps a `:tick` on the game
record, bumps it on every state change, and arms a timer that captures the tick
it was armed at. A timer left over from an action that already happened finds a
newer tick and does nothing. That is the whole mechanism; it needs no
cancellation and no bookkeeping.

## Wiring

1. `shadow-cljs.edn` — add a build with `:init-fn <game>.play/init!` and
   `:devtools {:after-load <game>.play/mount-components}`.
2. `handler.clj` — require and add `(<game>-routes db)` and `(<game>-ws-routes db)`.
3. **Add the build name to all four places it is spelled out**: `shadow-cljs.edn`,
   `README.md`, `project.clj`, and `deploy.sh`. Missing `deploy.sh` means the
   game works locally and ships with no JavaScript.

## Checks worth running

```
clj-kondo --lint src/...                       # after every Clojure edit
lein run -m clojure.main -e "(require '<ns>)"   # the namespaces actually load
npx shadow-cljs compile <game>                  # the cljs actually compiles
lein test <game>.<ns>-test
```

A rules namespace should have one test that is embarrassingly thorough — the
kind that enumerates rather than samples. `universe.deck-test` classifies all
5,461,512 five-card hands and checks the counts against the chart; that is 30
seconds once and it means the ranking is not a matter of opinion.
