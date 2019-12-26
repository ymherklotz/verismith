
# Table of Contents

1.  [Introduction](#org2e88aa0)
2.  [Finding failures in Yosys 0.8](#org7368fab)
    1.  [Installing Yosys master](#orgd073682)



<a id="org2e88aa0"></a>

# Introduction

The version of Verismith that is assumed to be used is Verismith 0.6.0.2, which is also available on [hackage](<https://hackage.haskell.org/package/verismith-0.6.0.2>) using:

    cabal install verismith


<a id="org7368fab"></a>

# Finding failures in Yosys 0.8

Yosys 0.8 was found to fail about 30% of the time, which means that it should be quite simple to find errors in it. However, different versions of Yosys can be tested this way as well and should also result in failures, such as Yosys 0.9 or Yosys commit hashes 3333e00 or 70d0f38.

However, to find failures in Yosys 0.8, a newer version of Yosys has to be used for the equivalence check. For this we can use Yosys master. An alternative for this is to use a simulator with a testbench, which is also supported by Verismith using Icarus Verilog.


<a id="orgd073682"></a>

## Installing Yosys master

The first step is to install Yosys master (which will in this case be installed to `/opt/yosys/master`):

    git clone https://github.com/yosyshq/yosys && cd yosys
    sed -i 's/^PREFIX ?=.*/PREFIX ?= /opt/yosys/master'
    make -j4
    sudo make install

Then we want to install Yosys 0.8 (which will be installed to `/opt/yosys/0.8`):

    git clean -dfx && git reset --hard HEAD
    git checkout yosys-0.8
    sed -i 's/^PREFIX ?=.*/PREFIX ?= /opt/yosys/0.8'
    make -j4
    sudo make install

