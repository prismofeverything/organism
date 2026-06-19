# syntax=docker/dockerfile:1.7
# Three stages:
#   builder — Debian-based, compiles cljs bundles + uberjar
#   jlink   — Alpine JDK, produces a musl-targeted minimal JRE from chosen modules
#   runtime — Alpine + minimal JRE + uberjar

# ── builder ───────────────────────────────────────────────────────────────────
FROM clojure:temurin-21-lein-bookworm AS builder

# Node.js for shadow-cljs.
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates gnupg \
 && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
 && apt-get install -y --no-install-recommends nodejs \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Prime dep caches before source so code edits don't bust them.
COPY project.clj shadow-cljs.edn package.json package-lock.json ./
RUN lein deps
RUN npm ci

COPY src       ./src
COPY env       ./env
COPY resources ./resources

# Compile ClojureScript bundles into resources/public/js/.
RUN npx shadow-cljs release organism journey journey-bots oroboros eridu future

# AOT-compile + uberjar.
RUN lein uberjar

# ── jlink: build a musl-targeted custom JRE ───────────────────────────────────
FROM eclipse-temurin:21-jdk-alpine AS jlink

# Hand-picked module set. jdeps misses reflection-loaded modules, so this is a
# slightly generous list that's safe for the Clojure runtime + this app's deps.
#
#   java.base               core
#   java.logging            clojure.tools.logging, slf4j
#   java.naming             slf4j, jndi-style lookups
#   java.sql                Selmer / transitive date formatting
#   java.management         JMX hooks used by some deps
#   java.desktop            thi.ng/color (color/image math)
#   java.net.http           modern HTTP client (transitive)
#   java.xml                XML config parsing in some deps
#   java.security.jgss      Kerberos/SASL (needed if Mongo SASL is ever enabled)
#   java.instrument         used by agents/devtools transitively
#   jdk.crypto.ec           TLS for mongodb+srv:// and HTTPS outbound
#   jdk.crypto.cryptoki     extra TLS algorithms
#   jdk.unsupported         sun.misc.Unsafe — used by many fast-collection libs
RUN jlink \
      --add-modules java.base,java.logging,java.naming,java.sql,java.management,java.desktop,java.net.http,java.xml,java.security.jgss,java.instrument,jdk.crypto.ec,jdk.crypto.cryptoki,jdk.unsupported \
      --strip-debug --no-man-pages --no-header-files \
      --compress=zip-9 \
      --output /opt/jre

# ── runtime: alpine + custom JRE + uberjar ────────────────────────────────────
FROM alpine:3.20

# musl JRE runtime requirements.
RUN apk add --no-cache libstdc++ zlib ca-certificates

ENV JAVA_HOME=/opt/jre
ENV PATH=/opt/jre/bin:$PATH

COPY --from=jlink   /opt/jre                                /opt/jre
COPY --from=builder /build/target/uberjar/organism.jar      /app/organism.jar

WORKDIR /app

ENV PORT=3000
EXPOSE 3000

ENTRYPOINT ["java", "-jar", "/app/organism.jar"]
