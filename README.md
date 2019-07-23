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

The fuzzer now supports building with [nix](https://nixos.org/nix/manual/),
which pulls in all the extra dependencies that are needed to build the
project. The main files and their functions are described below:

- `default.nix`: describes the main Haskell package and it's dependencies that
  have to be pulled in.
- `shell.nix`: describes how to set up a shell with `nix-shell` which has all
  the needed dependencies present.
- `release.nix`: passes the versions of the packages that should be used to the
  description of the fuzzer in `default.nix`, which also overrides some
  dependencies so that everything builds nicely. The exact versions of the
  packages that should be overridden are in [nix](/nix).

It may be possible to build it purely with
[cabal-install](https://hackage.haskell.org/package/cabal-install), however
it may not have all the right versions of the dependencies that are needed.

Instead, stack could be used and the `stack.yaml` file could contain the
overrides that are used by nix.

### Build with nix

Nix build is completely supported, therefore if nix is installed, building the
project is as simple as

``` shell
nix-build release.nix
```

If one wants to work in the project with all the right dependencies loaded, one
can use

``` shell
nix-shell
```

### Build with cabal and nix

After entering a development environment with `nix-shell`, the project can
safely be built with `cabal-install`.

``` shell
cabal v2-configure
cabal v2-build
```

This should not have to download any extra dependencies and just have to build
the actual project itself.

## Configuration

VeriFuzz can be configured using a [TOML](https://github.com/toml-lang/toml)
file. There are four main sections in the configuration file, an example can be
seen [here](/examples/config.toml).

### Information section 

Contains information about the command line tool being used, such as the hash of
the commit it was compiled with and the version of the tool. The tool then
verifies that these match the current configuration, and will emit a warning if
they do not. This ensures that if one wants a deterministic run and is therefore
passing a seed to the generation, that it will always give the same
result. Different versions might change some aspects of the Verilog generation,
which would affect how a seed would generate Verilog.

### Probability section 

Provides a way to assign frequency values to each of the nodes in the
AST. During the state-based generation, each node is chosen randomly based on
those probabilities. This provides a simple way to drastically change the
Verilog that is generated, by changing how often a construct is chosen or by not
generating a construct at all.

### Property section 

Changes properties of the generated Verilog code, such as the size of the
output, maximum statement or module depth and sampling method of Verilog
programs. This section also allows a seed to be specified, which would mean that
only that particular seed will be used in the fuzz run. This is extremely useful
when wanting to replay a specific failure and the output is missing.

### Synthesiser section 

Accepts a list of synthesisers which will be fuzzed. These have to first be
defined in the code and implement the required interface. They can then be
configured by having a name assigned to them and the name of the output Verilog
file. By each having a different name, multiple instances of the same
synthesiser can be included in a fuzz run. The instances might differ in the
optimisations that are performed, or in the version of the synthesiser.

## Acknowledgement

Clifford Wolf's [VlogHammer](http://www.clifford.at/yosys/vloghammer.html) is an
existing Verilog fuzzer that generates random Verilog expressions. It was the
inspiration for the general structure of this fuzzer, which extends the fuzzing
to the behavioural parts of Verilog.

Tom Hawkins' Verilog parser was used to write the lexer, the parser was then
rewritten using [Parsec](https://hackage.haskell.org/package/parsec).
