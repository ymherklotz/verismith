
# Table of Contents

1.  [Introduction](#org1b61a13)
2.  [Finding failures in Yosys 0.8](#org822a3f3)
    1.  [Installing Yosys master](#org1796c08)
    2.  [Running Verismith](#org4bcde97)
    3.  [Using a pre-existing seed](#org5ee7571)
3.  [Better example of reduction and interesting failure](#orgbd91ba7)
    1.  [Build Yosys 3333e002](#org866de1a)
    2.  [Running Verismith for mis-synthesis](#orgb24b2ef)
    3.  [Running Verismith for crash](#org51a0c9d)



<a id="org1b61a13"></a>

# Introduction

The version of Verismith that is assumed to be used is Verismith 0.6.0.2, which is also available on [hackage](<https://hackage.haskell.org/package/verismith-0.6.0.2>) using:

    cabal install verismith


<a id="org822a3f3"></a>

# Finding failures in Yosys 0.8

Yosys 0.8 was found to fail about 30% of the time, which means that it should be quite simple to find errors in it. However, different versions of Yosys can be tested this way as well and should also result in failures, such as Yosys 0.9 or Yosys commit hashes 3333e00 or 70d0f38.

However, to find failures in Yosys 0.8, a newer version of Yosys has to be used for the equivalence check. For this we can use Yosys master. An alternative for this is to use a simulator with a test bench, which is also supported by Verismith using Icarus Verilog.

**Note**: The most common error in Yosys 0.8 is regarding for loops, which are not dealt that well with the reducer at the moment.


<a id="org1796c08"></a>

## Installing Yosys master

The first step is to install Yosys master (which will in this case be installed to `/opt/yosys/master`):

    git clone https://github.com/yosyshq/yosys && cd yosys
    sed -i 's:^PREFIX ?=.*:PREFIX ?= /opt/yosys/master:' Makefile
    make -j4
    sudo make install

Then we want to install Yosys 0.8 (which will be installed to `/opt/yosys/0.8`):

    git clean -dfx && git reset --hard HEAD
    git checkout yosys-0.8
    sed -i 's:^PREFIX ?=.*:PREFIX ?= /opt/yosys/0.8:' Makefile
    make -j4
    sudo make install


<a id="org4bcde97"></a>

## Running Verismith

We are then ready to run Verismith using the two Yosys versions that were installed.

Using the following config file saved in `config.toml`:

    [info]
      commit = "UNKNOWN"
      version = "0.6.0.2"
    
    [probability]
      expr.binary = 5
      expr.concatenation = 3
      expr.number = 1
      expr.rangeselect = 5
      expr.signed = 5
      expr.string = 0
      expr.ternary = 5
      expr.unary = 5
      expr.unsigned = 5
      expr.variable = 5
      moditem.assign = 5
      moditem.combinational = 0
      moditem.instantiation = 1
      moditem.sequential = 1
      statement.blocking = 0
      statement.conditional = 1
      statement.forloop = 1
      statement.nonblocking = 5
    
    [property]
      determinism = 1
      module.depth = 2
      module.max = 5
      nondeterminism = 0
      output.combine = false
      sample.method = "hat"
      sample.size = 10
      size = 20
      statement.depth = 3
      default.yosys = "/opt/yosys/master/bin"
    
    [[synthesiser]]
      bin = "/opt/yosys/0.8/bin"
      description = "yosys_0_8"
      name = "yosys"
      output = "syn_yosys_0_8.v"
    
    [[synthesiser]]
      bin = "/opt/yosys/master/bin"
      description = "yosys_master"
      name = "yosys"
      output = "syn_yosys_master.v"

To run Verismith for 10 iterations, which should find a bug, we can run the following:

    verismith fuzz -c config.toml -n 10 -o yosys_output

Failures can then either be seen on the output, or a summary can be seen in the browser using the following:

    firefox yosys_output/index.html


<a id="org5ee7571"></a>

## Using a pre-existing seed

If a failure still cannot be found in Yosys 0.8 using the previous, it should be possible using the following config file, which will generate Verilog based on a seed that was found to produce an error in Yosys 0.8:

    
    [info]
      commit = "UNKNOWN"
      version = "0.6.0.2"
    
    [probability]
      expr.binary = 5
      expr.concatenation = 3
      expr.number = 1
      expr.rangeselect = 5
      expr.signed = 5
      expr.string = 0
      expr.ternary = 5
      expr.unary = 5
      expr.unsigned = 5
      expr.variable = 5
      moditem.assign = 5
      moditem.combinational = 1
      moditem.instantiation = 1
      moditem.sequential = 1
      statement.blocking = 0
      statement.conditional = 1
      statement.forloop = 1
      statement.nonblocking = 5
    
    [property]
      default.yosys = "/opt/yosys/master/bin"
      determinism = 1
      module.depth = 2
      module.max = 5
      nondeterminism = 0
      output.combine = false
      sample.method = "hat"
      sample.size = 10
      seed = "Seed 17999570691447884947 12099254006121016321"
      size = 20
      statement.depth = 3
    
    [[synthesiser]]
      bin = "/opt/yosys/0.8/bin"
      description = "yosys_0_8"
      name = "yosys"
      output = "syn_yosys_0_8.v"
    
    [[synthesiser]]
      bin = "/opt/yosys/master/bin"
      description = "yosys_master"
      name = "yosys"
      output = "syn_yosys_master.v"

Just save the config file in `config.toml` and run the following:

    verismith fuzz -c config.toml -o yosys_one

Which should find a bug and reduce it to around 200 loc out of 1000.


<a id="orgbd91ba7"></a>

# Better example of reduction and interesting failure

This bug was found in a development version of Yosys (commit hash 3333e002) and was [reported and fixed in Yosys](<https://github.com/YosysHQ/yosys/issues/997>). In addition to that, a crash can also be reproduced which was also [reported and fixed in Yosys](<https://github.com/YosysHQ/yosys/issues/993>).


<a id="org866de1a"></a>

## Build Yosys 3333e002

First, we need to build Yosys 3333e002, in addition to the version of Yosys master [built earlier](#org1796c08).

    git clean -dfx && git reset --hard HEAD
    git checkout 3333e002 -b test
    sed -i 's:^PREFIX ?=.*:PREFIX ?= /opt/yosys/3333e002:' Makefile
    make -j4
    sudo make install


<a id="orgb24b2ef"></a>

## Running Verismith for mis-synthesis

Save the following config file in `config.toml`:

    [info]
      commit = "UNKNOWN"
      version = "0.6.0.2"
    
    [probability]
      expr.binary = 5
      expr.concatenation = 3
      expr.number = 1
      expr.rangeselect = 5
      expr.signed = 5
      expr.string = 0
      expr.ternary = 5
      expr.unary = 5
      expr.unsigned = 5
      expr.variable = 5
      moditem.assign = 5
      moditem.combinational = 1
      moditem.instantiation = 1
      moditem.sequential = 1
      statement.blocking = 0
      statement.conditional = 1
      statement.forloop = 0
      statement.nonblocking = 3
    
    [property]
      default.yosys = "/opt/yosys/master/bin"
      determinism = 1
      module.depth = 2
      module.max = 5
      nondeterminism = 0
      output.combine = false
      sample.method = "random"
      sample.size = 10
      seed = "Seed 6762640716476645086 15760899726111280279"
      size = 20
      statement.depth = 3
    
    [[synthesiser]]
      bin = "/opt/yosys/3333e002/bin"
      description = "yosys"
      name = "yosys"
      output = "syn_yosys.v"

Then run Verismith with the following:

    verismith fuzz -c config.toml -o output_ms

The result should be that the equivalence check fails and a reduced testcase should be available in `output_ms/fuzz_1/reduce_identity_yosys.v`.

Contrary to what is expected, the simulation runs will pass. This is because the bug occurs in the initial values that are assigned to the variables. These are set to 0 in the design, but mistakenly set to x in the synthesised design. The testbench does not check for those values and the error is therefore not found by the testbench.

To fix this manually, one can add a `$strobe("%b", y);` on line 22 in the yosys testbench:

    cd output_ms/fuzz_1/simulation_yosys
    sed -i '21 a      $strobe("%b", y);' yosys_testbench.v
    iverilog -o yosys_main yosys_testbench.v
    # ./yosys_main | grep 'x'
    ./yosys_main

which should show some `x` in the output which should not be there.


<a id="org51a0c9d"></a>

## Running Verismith for crash

**Note**: Verismith is not that great at reducing crashes, as is explained in our paper.

Save the following config file in `config.toml`:

    [info]
      commit = "UNKNOWN"
      version = "0.6.0.2"
    
    [probability]
      expr.binary = 5
      expr.concatenation = 3
      expr.number = 1
      expr.rangeselect = 5
      expr.signed = 5
      expr.string = 0
      expr.ternary = 5
      expr.unary = 5
      expr.unsigned = 5
      expr.variable = 5
      moditem.assign = 5
      moditem.combinational = 1
      moditem.instantiation = 1
      moditem.sequential = 1
      statement.blocking = 0
      statement.conditional = 1
      statement.forloop = 0
      statement.nonblocking = 3
    
    [property]
      default.yosys = "/opt/yosys/master/bin"
      determinism = 1
      module.depth = 2
      module.max = 5
      nondeterminism = 0
      output.combine = false
      sample.method = "random"
      sample.size = 10
      seed = "Seed 10125302424574354942 828176532243040297"
      size = 20
      statement.depth = 3
    
    [[synthesiser]]
      bin = "/opt/yosys/3333e002/bin"
      description = "yosys"
      name = "yosys"
      output = "syn_yosys.v"

Then run Verismith with the following:

    verismith fuzz -c config.toml -o output_c

