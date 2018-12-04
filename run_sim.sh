#!/usr/bin/env bash

while true; do
  .stack-work/install/x86_64-linux-tinfo6/lts-12.20/8.4.4/bin/verifuzz > main.v
  iverilog main.v
  vvp main.v
done
