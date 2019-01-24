# VeriFuzz [![Build Status](https://travis-ci.com/ymherklotz/verifuzz.svg?token=qfBKKGwxeWkjDsy7e16x&branch=master)](https://travis-ci.com/ymherklotz/verifuzz)

Verilog Fuzzer to test the major verilog compilers by generating random, valid
verilog.

It currently supports the following simulators:

- [Yosys](http://www.clifford.at/yosys/)
- [Icarus Verilog](http://iverilog.icarus.com)
- [Xst](https://www.xilinx.com/support/documentation/sw_manuals/xilinx11/ise_c_using_xst_for_synthesis.htm)

## Build the Fuzzer

The fuzzer is split into an executable (in the [app](/app) folder) and a
library (in the [src](/src) folder). To build the executable, you will need
[stack](https://docs.haskellstack.org/en/stable/README/) installed. Building
directly using [cabal-install](https://www.haskell.org/cabal/download.html) is
possible but not recommended and not directly supported.

To build the executable:

```
stack build
```

To run the executable:

```
stack exec verifuzz
```

To install the executable (which defaults to installing it in `~/.local`):

```
stack install
```

## Running tests

There are two test-suites that currently test the library. One of the
test-suites tests the random code generation and generation of the acyclic
graph. It also contains some property based tests for this. The other test-suite
uses `doctest` to test the examples that are in the documentation.

To run the test-suites:

```
stack test
```
