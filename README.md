# organism

A server for synchronous and asynchronous play of ORGANISM, the board game.

![ORGANISM](https://github.com/prismofeverything/organism/blob/master/resources/public/img/organism-five-player.png)

An instance is currently running at https://playorganism.io/

## Prerequisites

- [Leiningen][1] 2.0+
- Node.js / npm (for shadow-cljs)
- MongoDB running locally on the default port

[1]: https://github.com/technomancy/leiningen

## Local development

Create a `dev-config.edn` in the project root (gitignored):

    {:dev true
     :port 3000
     :nrepl-port 7000}

Install JS deps once:

    npm install

Then run two processes in parallel:

1. **ClojureScript build (hot-reload):**

        npx shadow-cljs watch organism journey oroboros eridu future

2. **Clojure server:**

        lein run

The server listens on `http://localhost:3000` and starts an nREPL on `7000`. Shadow writes compiled JS into `resources/public/js/`, which the server serves directly.

## Production build

    npx shadow-cljs release organism journey oroboros eridu future
    lein uberjar

The uberjar at `target/uberjar/organism.jar` includes the release JS bundle.

## License

Copyright © 2021 Ryan Spangler
