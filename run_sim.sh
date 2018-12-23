#!/usr/bin/env bash

stack build verifuzz

while true; do
  stack exec verifuzz > main.v
  iverilog main.v -o main
  vvp main
done
