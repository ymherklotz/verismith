#!/usr/bin/env bash

base=$(dirname $1)

current1=$(find ${base} -maxdepth 1 -mindepth 1 -name 'reduce_sim_yosys_0_8.v')
current2=$(find ${base} -maxdepth 1 -mindepth 1 -name 'reduce_sim_yosys_0_9.v')
current3=$(find ${base} -maxdepth 1 -mindepth 1 -name 'reduce_sim_yosys_master.v')
current4=$(find ${base} -maxdepth 1 -mindepth 1 -name 'reduce_equiv_yosys_master.v')
current5=$(find ${base} -maxdepth 1 -mindepth 1 -name 'reduce_equiv_yosys_0_8.v')
current6=$(find ${base} -maxdepth 1 -mindepth 1 -name 'reduce_equiv_yosys_0_9.v')

if [[ $current1 != "" ]] && [[ $current2 != "" ]] && [[ $current3 != "" ]] &&
   [[ $current4 = "" ]] && [[ $current4 = "" ]] && [[ $current5 = "" ]]; then
  echo $base
fi
