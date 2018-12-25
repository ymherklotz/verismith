#!/usr/bin/env bash

stack build verifuzz
stack install

while true; do
  verifuzz > main.v
  iverilog main.v -o main
  vvp main
done
