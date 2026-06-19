# organism

A web server for synchronous and asynchronous play of several distinct board
games. Organism was the first implemented (the repo is named after it); the
server now also hosts journey, oroboros, eridu, and future. Each is its own
design. They share a Clojure backend, a ClojureScript SPA, and MongoDB for
persistence.

![ORGANISM](https://github.com/prismofeverything/organism/blob/master/resources/public/img/organism-five-player.png)

An instance is currently running at https://playorganism.io/

## Run locally with Docker

The fastest way to try the server — no Clojure/Java/Node toolchain needed on the host:

    docker compose up --build

Open http://localhost:3000.

`docker compose` brings up two containers:

- **app** — multi-stage build: shadow-cljs + `lein uberjar` run inside the image,
  the runtime is Alpine + a `jlink`'d JRE + the uberjar (~165 MB per arch).
- **mongo** — `mongo:7` with a named volume (`mongo_data`) for persistence across
  restarts. Exposed on `localhost:27017` so you can poke at the data with `mongosh`.

Useful commands:

    docker compose up -d            # start detached
    docker compose logs -f app      # tail the server
    docker compose down             # stop (keeps the mongo volume)
    docker compose down -v          # stop and wipe the mongo volume
    docker compose up -d --build    # rebuild after a code change

The Mongo connection reads `MONGO_HOST`, `MONGO_PORT`, and `MONGO_DB` from the
environment (defaulting to `localhost`, `27017`, `organism`), so the same image
works with an external Mongo by setting those vars.

## Local development (native, with hot reload)

If you want a REPL and ClojureScript hot-reload, run the toolchain directly.

Prerequisites:

- [Leiningen][1] 2.0+
- Node.js / npm (for shadow-cljs)
- MongoDB running locally on the default port (or set `MONGO_HOST` / `MONGO_PORT`)

Create a `dev-config.edn` in the project root (gitignored):

    {:dev true
     :port 3000
     :nrepl-port 7000}

Install JS deps once:

    npm install

Then run two processes in parallel:

1. **ClojureScript build (hot-reload):**

        npx shadow-cljs watch organism journey journey-bots oroboros eridu future

2. **Clojure server:**

        lein run

The server listens on `http://localhost:3000` and starts an nREPL on `7000`.
Shadow writes compiled JS into `resources/public/js/`, which the server serves
directly.

[1]: https://github.com/technomancy/leiningen

## Production build

    npx shadow-cljs release organism journey journey-bots oroboros eridu future
    lein uberjar

The uberjar at `target/uberjar/organism.jar` bundles the release JS.

## Research code

The top-level `alphazero/` directory is a Python research apparatus —
AlphaZero training for Organism and Journey, and the **Oroboros** procedural
game-design search. It is independent of the running server and is excluded
from the docker image. See `alphazero/requirements.txt`.

## License

Copyright © 2021 Ryan Spangler