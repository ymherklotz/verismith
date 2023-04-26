# Verismith

[![Build Status](https://travis-ci.com/ymherklotz/verismith.svg?token=qfBKKGwxeWkjDsy7e16x&branch=master)](https://travis-ci.com/ymherklotz/verismith)
[![FPGA'20 DOI](https://img.shields.io/badge/FPGA'20%20DOI-10.1145%2F3373087.3375310-blue)](https://doi.org/10.1145/3373087.3375310)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3598790.svg)](https://doi.org/10.5281/zenodo.3598790)
[![Hackage](https://img.shields.io/hackage/v/verismith?color=614f88)](http://hackage.haskell.org/package/verismith)

Verilog Fuzzer to test the major verilog compilers by generating random, valid and deterministic Verilog.

It currently supports the following synthesis tools:

- [Yosys](http://www.clifford.at/yosys/)
- [Xst](https://www.xilinx.com/support/documentation/sw_manuals/xilinx11/ise_c_using_xst_for_synthesis.htm)
- [Vivado](https://www.xilinx.com/products/design-tools/ise-design-suite.html)
- [Quartus](https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html)

and the following simulator:

- [Icarus Verilog](http://iverilog.icarus.com)

## Supported Verilog Constructs

The fuzzer generates combinational and behavioural Verilog to test the various tools. The most notable constructs that are supported and generated are the following:

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

11 bugs have been reported and confirmed to be bugs by the vendors, out of which 4 have been fixed. 1 bug has also been found in the Icarus Verilog simulator as a side effect of using it to verify equivalence check results.

### Yosys

| Type          | Issue                                                      | Confirmed | Fixed |
|---------------|------------------------------------------------------------|-----------|-------|
| Mis-synthesis | [Issue 1531](https://github.com/YosysHQ/yosys/issues/1531) | ✓         | ✓     |
| Mis-synthesis | [Issue 1243](https://github.com/YosysHQ/yosys/issues/1243) | ✓         | ✓     |
| Mis-synthesis | [Issue 1047](https://github.com/YosysHQ/yosys/issues/1047) | ✓         | ✓     |
| Mis-synthesis | [Issue 997](https://github.com/YosysHQ/yosys/issues/997)   | ✓         | ✓     |
| Crash         | [Issue 993](https://github.com/YosysHQ/yosys/issues/993)   | ✓         | ✓     |

### Vivado

| Type          | Issue                                                                                                                               | Confirmed | Fixed |
|---------------|-------------------------------------------------------------------------------------------------------------------------------------|-----------|-------|
| Crash         | [Forum 981787](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Verilog-If-statement-nesting-crash/td-p/981787)                 | ✓         | ✓     |
| Crash         | [Forum 981136](https://forums.xilinx.com/t5/Synthesis/Vivado-2018-3-synthesis-crash/td-p/981136)                                    | ✓         | ✓     |
| Mis-synthesis | [Forum 981789](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Unsigned-bit-extension-in-if-statement/td-p/981789)             | ✓         | ✓     |
| Mis-synthesis | [Forum 982518](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Signed-with-shift-in-condition-synthesis-mistmatch/td-p/982518) | ✓         | ✓     |
| Mis-synthesis | [Forum 982419](https://forums.xilinx.com/t5/Synthesis/Vivado-2019-1-Bit-selection-synthesis-mismatch/td-p/982419)                   | ✓         | ✓     |

### Icarus Verilog

| Type           | Issue                                                           | Confirmed | Fixed |
|----------------|-----------------------------------------------------------------|-----------|-------|
| Mis-simulation | [Issue 283](https://github.com/steveicarus/iverilog/issues/283) | ✓         | ✓     |

## Install the Fuzzer

The fuzzer now supports building with [nix](https://nixos.org/nix/manual/), which pulls in all the extra dependencies that are needed to build the project. The main files and their functions are described below:

- `default.nix`: describes the main Haskell package and it's dependencies that
  have to be pulled in.
- `shell.nix`: describes how to set up a shell with `nix-shell` which has all
  the needed dependencies present.
- `release.nix`: passes the versions of the packages that should be used to the
  description of the fuzzer in `default.nix`, which also overrides some
  dependencies so that everything builds nicely. The exact versions of the
  packages that should be overridden are in [nix](/nix).

It may be possible to build it purely with [cabal-install](https://hackage.haskell.org/package/cabal-install), however it may not have all the right versions of the dependencies that are needed.

Instead, stack could be used and the `stack.yaml` file could contain the overrides that are used by nix.

### Build from hackage

Some external packages are required to use Verismith properly:

- [Yosys](https://github.com/yosyshq/yosys) with ABC
- [SymbiYosys](https://github.com/YosysHQ/SymbiYosys)
- [Icarus Verilog](http://iverilog.icarus.com/)
- (Optional) [Z3](https://github.com/Z3Prover/z3)
- (Optional) [Xst](https://www.xilinx.com/support/documentation/sw_manuals/xilinx11/ise_c_using_xst_for_synthesis.htm)
- (Optional) [Vivado](https://www.xilinx.com/products/design-tools/ise-design-suite.html)
- (Optional) [Quartus](https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html)

A stable version of Verismith is available on [hackage](https://hackage.haskell.org/package/verismith) and can be installed using cabal directly without having to build the project from the repository:

**Note**: Only **GHC 8.6.5-8.8.2** are currently supported.

``` shell
cabal install verismith
```

It will be placed under the `bin` cabal folder which can be added to your path to run Verismith.

### Build with nix from source

Nix build is completely supported, therefore if nix is installed, building the project is as simple as

``` shell
nix-build
```

If one wants to work in the project with all the right dependencies loaded, one can use

``` shell
nix-shell
```

and use cabal to build and run the program.

### Build with cabal from source

After entering a development environment with `nix-shell`, the project can safely be built with `cabal-install`. However, even without `nix`, the project can still be built with cabal alone using:

``` shell
cabal update     # needed if cabal is used for the first time
cabal configure
cabal build
```

Verismith can then be run using:

``` shell
cabal run verismith
```

## Configuration

Verismith can be configured using a [TOML](https://github.com/toml-lang/toml) file. There are four main sections in the configuration file, an example can be seen [here](/examples/config.toml).

### Information section

Contains information about the command line tool being used, such as the hash of the commit it was compiled with and the version of the tool. The tool then verifies that these match the current configuration, and will emit a warning if they do not. This ensures that if one wants a deterministic run and is therefore passing a seed to the generation, that it will always give the same result. Different versions might change some aspects of the Verilog generation, which would affect how a seed would generate Verilog.

### Probability section

Provides a way to assign frequency values to each of the nodes in the AST. During the state-based generation, each node is chosen randomly based on those probabilities. This provides a simple way to drastically change the Verilog that is generated, by changing how often a construct is chosen or by not generating a construct at all.

### Property section

Changes properties of the generated Verilog code, such as the size of the output, maximum statement or module depth and sampling method of Verilog programs. This section also allows a seed to be specified, which would mean that only that particular seed will be used in the fuzz run. This is extremely useful when wanting to replay a specific failure and the output is missing.

### Synthesiser section

Accepts a list of synthesisers which will be fuzzed. These have to first be defined in the code and implement the required interface. They can then be configured by having a name assigned to them and the name of the output Verilog file. By each having a different name, multiple instances of the same synthesiser can be included in a fuzz run. The instances might differ in the optimisations that are performed, or in the version of the synthesiser.

## Benchmark Results

Current benchmark results to compare against.

``` text
benchmarking generation/default
time                 65.16 ms   (42.67 ms .. 84.90 ms)
                     0.837 R²   (0.722 R² .. 0.966 R²)
mean                 82.87 ms   (71.13 ms .. 105.9 ms)
std dev              27.59 ms   (15.80 ms .. 42.35 ms)
variance introduced by outliers: 90% (severely inflated)

benchmarking generation/depth
time                 860.8 ms   (2.031 ms .. 1.488 s)
                     0.900 R²   (0.668 R² .. 1.000 R²)
mean                 483.9 ms   (254.1 ms .. 647.6 ms)
std dev              224.4 ms   (100.8 ms .. 283.5 ms)
variance introduced by outliers: 74% (severely inflated)

benchmarking generation/size
time                 541.1 ms   (-749.1 ms .. 1.263 s)
                     0.568 R²   (0.005 R² .. 1.000 R²)
mean                 698.8 ms   (498.2 ms .. 897.5 ms)
std dev              229.8 ms   (195.0 ms .. 239.7 ms)
variance introduced by outliers: 73% (severely inflated)

```

## Resources

The following resources provide more details about the in depth implementation of Verismith:

- [Verismith FPGA '20](https://yannherklotz.com/docs/verismith/verismith_paper.pdf)
- [Verismith thesis](https://yannherklotz.com/docs/verismith/verismith_thesis.pdf)
- [Verismith slides](https://yannherklotz.com/docs/verismith/verismith_slides.pdf): Presented to the Circuits and Systems group at Imperial College on the 01/07/2019.
- [Verismith poster](https://yannherklotz.com/docs/verismith/verismith_poster.pdf): Presented at the [Microsoft Research PhD Workshop](https://www.microsoft.com/en-us/research/event/phd-workshop-on-next-generation-cloud-infrastructure/) on 25/11/2019.

## Publication

If you use Verismith in your research, please cite our [FPGA '20 paper](https://yannherklotz.com/docs/verismith/verismith_paper.pdf)

``` text
@inproceedings{herklotz_verismith_fpga2020,
  author = {Yann Herklotz and John Wickerson},
  title = {Finding and Understanding Bugs in {FPGA} Synthesis Tools},
  year = 2020,
  booktitle = {ACM/SIGDA Int. Symp. on Field-Programmable Gate Arrays},
  doi = {10.1145/3373087.3375310},
  isbn = {978-1-4503-7099-8/20/02},
  keywords = {automated testing, compiler defect, compiler testing, random program generation, random testing},
  location = {Seaside, CA, USA},
  numpages = 11,
  publisher = {ACM},
  series = {FPGA '20},
}
```

## Contributing

### Running the [ormolu formatter](https://github.com/tweag/ormolu).

It can be installed using the following:

```shell
cabal install ormolu
```

Then, it can be run on all the files in this repository using (in bash):

```shell
ormolu --mode inplace $(find src test app -name '*.hs')
```

## License

This open source version of Verismith is licensed under the GPLv3 license, which can be seen in the [LICENSE](/LICENSE) file.

A **closed source** version of Verismith is also available without the restrictions of the GPLv3 license following a software with a software usage agreement from Imperial College London.

``` text
Verismith: Verilog hardware synthesis tool fuzzer
Copyright (C) 2019-2020  Yann Herklotz <yann@yannherklotz.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

## Acknowledgement

Clifford Wolf's [VlogHammer](http://www.clifford.at/yosys/vloghammer.html) is an existing Verilog fuzzer that generates random Verilog to test how expressions are handled in synthesis tools and simulators. It was the inspiration for thegeneral structure of this fuzzer, which extends the fuzzing to the behavioural parts of Verilog.

Tom Hawkins' Verilog parser was used to write the lexer, the parser was then rewritten using [Parsec](https://hackage.haskell.org/package/parsec).
