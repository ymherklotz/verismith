# VeriFuzz [![Build Status](https://travis-ci.com/ymherklotz/verifuzz.svg?token=qfBKKGwxeWkjDsy7e16x&branch=master)](https://travis-ci.com/ymherklotz/verifuzz)

Verilog Fuzzer to test the major verilog compilers by generating random, valid
and deterministic Verilog. There is a
[presentation](https://yannherklotz.com/docs/presentation.pdf) about VeriFuzz
and a [thesis](https://yannherklotz.com/docs/thesis.pdf) which goes over all the
details of the implementation and results that were found.

It currently supports the following synthesis tools:

- [Yosys](http://www.clifford.at/yosys/)
- [Xst](https://www.xilinx.com/support/documentation/sw_manuals/xilinx11/ise_c_using_xst_for_synthesis.htm)
- [Vivado](https://www.xilinx.com/products/design-tools/ise-design-suite.html)
- [Quartus](https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html)

and the following simulator:

- [Icarus Verilog](http://iverilog.icarus.com)

## Supported Verilog Constructs

The fuzzer generates combinational and behavioural Verilog to test the various
tools. The most notable constructs that are supported and generated are the
following:

- module definitions with parameter definitions, inputs and outputs
- module items, such as instantiations, continuous assignment, always blocks,
  initial blocks, parameter and local parameter declarations
- most expressions, for example concatenation, arithmetic operations, ternary
    conditional operator
- behavioural code in sequential always blocks
- behavioural control flow such as if-else and for loops
- declaration of wires and variables of any size, signed or unsigned
- bit selection from wires and variables

## Reported bugs

21 bugs were found in total over the course of a month. 8 of those bugs were
reported and 3 were fixed.

### Yosys

| Type          | Issue                                                      | Confirmed | Fixed |
|---------------|------------------------------------------------------------|-----------|-------|
| Mis-synthesis | [Issue 1047](https://github.com/YosysHQ/yosys/issues/1047) | ✓         | ✓     |
| Mis-synthesis | [Issue 997](https://github.com/YosysHQ/yosys/issues/997)   | ✓         | ✓     |
| Crash         | [Issue 993](https://github.com/YosysHQ/yosys/issues/993)   | ✓         | ✓     |

### Vivado

| Type          | Issue                                                                                                                               | Confirmed | Fixed |
|---------------|-------------------------------------------------------------------------------------------------------------------------------------|-----------|-------|
| Crash         | [Forum 981787](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Verilog-If-statement-nesting-crash/td-p/981787)                 | ✓         | 𐄂     |
| Crash         | [Forum 981136](https://forums.xilinx.com/t5/Synthesis/Vivado-2018-3-synthesis-crash/td-p/981136)                                    | ✓         | 𐄂     |
| Mis-synthesis | [Forum 981789](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Unsigned-bit-extension-in-if-statement/td-p/981789)             | ✓         | 𐄂     |
| Mis-synthesis | [Forum 982518](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Signed-with-shift-in-condition-synthesis-mistmatch/td-p/982518) | ✓         | 𐄂     |
| Mis-synthesis | [Forum 982419](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Bit-selection-synthesis-mismatch/td-p/982419)                   | ✓         | 𐄂     |

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

## Acknowledgement

Clifford Wolf's [VlogHammer](http://www.clifford.at/yosys/vloghammer.html) is an
existing Verilog fuzzer that generates random Verilog expressions. It was the
inspiration for the general structure of this fuzzer, which extends the fuzzing
to the behavioural parts of Verilog.

Tom Hawkins' Verilog parser was used to write the lexer, the parser was then
rewritten using [Parsec](https://hackage.haskell.org/package/parsec).
