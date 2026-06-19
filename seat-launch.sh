#!/usr/bin/env bash
# Launch the claude-seat harness with a persistent FIFO so one JVM serves
# the whole game. Commands are sent by appending lines to /tmp/eridu_in;
# output accumulates in /tmp/eridu_out.
set -e
cd /home/m/organism
rm -f /tmp/eridu_in /tmp/eridu_out
mkfifo /tmp/eridu_in
# Holder keeps a write FD open so the reader never sees EOF.
sleep 100000 > /tmp/eridu_in &
echo "$!" > /tmp/eridu_holder.pid
exec /home/m/bin/lein run -m eridu.claude-seat < /tmp/eridu_in > /tmp/eridu_out 2>&1
