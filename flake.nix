{
  description = "ORGANISM — multi-game server (organism / journey / eridu / oroboros / future)";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # mongodb-ce is SSPL → marked unfree in nixpkgs.
          config.allowUnfree = true;
        };

        jdk   = pkgs.jdk21_headless;
        mongo = pkgs.mongodb-ce;

        # A local Mongo instance lives under ./.nix-mongo/ so it doesn't
        # collide with any system mongod.
        startMongo = pkgs.writeShellScriptBin "start-mongo" ''
          set -e
          ROOT="$PWD/.nix-mongo"
          mkdir -p "$ROOT/data" "$ROOT/log"
          if [ -f "$ROOT/mongod.pid" ] && kill -0 "$(cat $ROOT/mongod.pid)" 2>/dev/null; then
            echo "mongod already running (pid $(cat $ROOT/mongod.pid)) on :27017"
            exit 0
          fi
          ${mongo}/bin/mongod \
            --dbpath       "$ROOT/data" \
            --logpath      "$ROOT/log/mongod.log" \
            --pidfilepath  "$ROOT/mongod.pid" \
            --port 27017 \
            --bind_ip 127.0.0.1 \
            --fork
          echo "mongod started on 127.0.0.1:27017 (data: $ROOT/data, log: $ROOT/log/mongod.log)"
        '';

        stopMongo = pkgs.writeShellScriptBin "stop-mongo" ''
          ROOT="$PWD/.nix-mongo"
          if [ -f "$ROOT/mongod.pid" ]; then
            PID="$(cat $ROOT/mongod.pid)"
            if kill -0 "$PID" 2>/dev/null; then
              kill "$PID"
              # wait up to 10s for graceful exit
              for _ in 1 2 3 4 5 6 7 8 9 10; do
                kill -0 "$PID" 2>/dev/null || break
                sleep 1
              done
              echo "mongod stopped (was pid $PID)"
            else
              echo "stale pidfile — mongod not running"
            fi
            rm -f "$ROOT/mongod.pid"
          else
            echo "no $ROOT/mongod.pid — nothing to stop"
          fi
        '';

        mongoStatus = pkgs.writeShellScriptBin "mongo-status" ''
          ROOT="$PWD/.nix-mongo"
          if [ -f "$ROOT/mongod.pid" ] && kill -0 "$(cat $ROOT/mongod.pid)" 2>/dev/null; then
            echo "mongod running (pid $(cat $ROOT/mongod.pid)) on :27017"
          else
            echo "mongod NOT running"
          fi
        '';
      in {
        devShells.default = pkgs.mkShell {
          packages = [
            jdk
            pkgs.leiningen
            pkgs.clojure
            pkgs.clj-kondo
            pkgs.nodejs_22
            mongo
            pkgs.mongosh
            startMongo
            stopMongo
            mongoStatus
            pkgs.babashka
            # Headless browser for perf profiling of the CLJS UI.
            pkgs.chromium
          ];

          # puppeteer-core will spawn this binary instead of downloading
          # its own Chromium.
          PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";
          PUPPETEER_SKIP_DOWNLOAD  = "1";

          JAVA_HOME = "${jdk}/lib/openjdk";

          shellHook = ''
            echo "  organism dev shell"
            echo "  ──────────────────"
            printf "  java       %s\n" "$(java -version 2>&1 | head -1)"
            printf "  lein       %s\n" "$(lein -v 2>&1 | head -1)"
            printf "  clj        %s\n" "$(clojure -version 2>&1 | head -1)"
            printf "  node       %s\n" "$(node --version)"
            printf "  npx        %s\n" "$(npx --version)"
            printf "  mongod     %s\n" "$(mongod --version 2>&1 | head -1)"
            printf "  clj-kondo  %s\n" "$(clj-kondo --version 2>&1 | head -1)"
            echo
            echo "  Local Mongo (under ./.nix-mongo/):"
            echo "    start-mongo / stop-mongo / mongo-status"
            echo
            echo "  Dev workflow:"
            echo "    start-mongo                       # one-time per dev session"
            echo "    npx shadow-cljs watch \\"
            echo "        organism journey oroboros eridu future journey-bots"
            echo "    lein run                          # ring server, port 11551"
          '';
        };
      });
}
