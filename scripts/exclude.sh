#!/usr/bin/env bash

# bug0: ./output_vivado_all2/fuzz_8/vivado_2018.2/hs_err_pid108202.log
bug0="HOptGenControl::updateConst\(UConst\*, int, URange const&, UConst::Type, UConst::Type\)\+0x8d"

# bug1: ./output_vivado_all2/fuzz_18/vivado_2016.2/hs_err_pid128529.log
bug1="HOptDfg::reconnectLoadPinToSource\(DFPin\*, DFPin\*\)\+0x247"

# bug2: ./output_vivado_all2/fuzz_1/vivado_2016.2/hs_err_pid99371.log
bug2="HOptDfg::reconnectLoadPinToSource\(DFPin\*, DFPin\*\)\+0x2c0"

# bug3: ./output_vivado_all2/fuzz_1/vivado_2018.2/hs_err_pid99120.log
bug3="HOptDfg::reconnectLoadPinToSource\(DFPin\*, DFPin\*\)\+0x23b"

# bug4: ./size_test_length_no_combine/1/output5/fuzz_20/reduce_vivado/hs_err_pid52393.log
bug4="HOptDfg::mergeReconvergentPartitions\(DFPin\*, DFGraph\*, UHashSet<DFPin\*, DFPin\*, UEKey<DFPin\*>, UHashSetNode<DFPin\*>, DFPin\*, UEValue<DFPin\*, UHashSetNode<DFPin\*> > > const&, UHashMap<DFNode\*, DFPin\*, DFNode\*, UEKey<DFNode\*> >&, UHashMap<DFGraph\*, DFGraphInfo, DFGraph\*, UEKey<DFGraph\*> >&, DFGraph::DFGraphType, UHashMapList<DFGraph\*, UHashList<DFPin\*, DFPin\*, UEKey<DFPin\*>, UHashListNode<DFPin\*>, DFPin\*>, DFGraph\*, UEKey<DFGraph\*> >&\)\+0x1c6"

# bug5: ./swarm/medium95/fuzz_14/reduce_vivado/hs_err_pid126430.log
bug5="HOptDfg::mergeReconvergentPartitions\(DFPin\*, DFGraph\*, UHashSet<DFPin\*, DFPin\*, UEKey<DFPin\*>, UHashSetNode<DFPin\*>, DFPin\*, UEValue<DFPin\*, UHashSetNode<DFPin\*> > > const&, UHashMap<DFNode\*, DFPin\*, DFNode\*, UEKey<DFNode\*> >&, UHashMap<DFGraph\*, DFGraphInfo, DFGraph\*, UEKey<DFGraph\*> >&, DFGraph::DFGraphType, UHashMapList<DFGraph\*, UHashList<DFPin\*, DFPin\*, UEKey<DFPin\*>, UHashListNode<DFPin\*>, DFPin\*>, DFGraph\*, UEKey<DFGraph\*> >&\)\+0x2e8"

# bug6: ./swarm/medium108/fuzz_3/reduce_vivado/hs_err_pid95577.log
bug6="DD::DD\(Cudd\*, DdNode\*\)\+0x2a"

# bug7: ./output_vivado/medium13/fuzz_12/vivado_2016.2/hs_err_pid47970.log
bug7="HOptDfg::mergeReconvergentPartitions\(DFPin\*, DFGraph\*, UHashSet<DFPin\*, DFPin\*, UEKey<DFPin\*>, UHashSetNode<DFPin\*>, DFPin\*, UEValue<DFPin\*, UHashSetNode<DFPin\*> > > const&, UHashMap<DFNode\*, DFPin\*, DFNode\*, UEKey<DFNode\*> >&, UHashMap<DFGraph\*, DFGraphInfo, DFGraph\*, UEKey<DFGraph\*> >&, DFGraph::DFGraphType, UHashMapList<DFGraph\*, UHashList<DFPin\*, DFPin\*, UEKey<DFPin\*>, UHashListNode<DFPin\*>, DFPin\*>, DFGraph\*, UEKey<DFGraph\*> >&\)\+0x241"

# bug8: ./output_vivado/medium10/fuzz_5/vivado_2016.2/hs_err_pid50009.log
bug8="HOptGenControl::extractSyncRSForWireOrMerge\(DFGraph\*, DFNode\*, URange const&, DFEdge\*, DFPin\*\*, UConst\*, int, UConst\*, int, bool, DFPin\*&, UHashSet<DFNode\*, DFNode\*, UEKey<DFNode\*>, UHashSetNode<DFNode\*>, DFNode\*, UEValue<DFNode\*, UHashSetNode<DFNode\*> > >&, bool\)\+0xc33"

grep -E "$bug0|$bug1|$bug2|$bug3|$bug4|$bug5|$bug6|$bug7|$bug8" $1 >/dev/null 2>&1
exitcode=$?

if [[ $exitcode -ne 0 ]]; then
  echo $1
  head $1
  echo
fi
