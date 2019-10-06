{ nixpkgs ? null, compiler ? "ghc865", doBenchmark ? false } :
let
  sysPkg = import <nixpkgs> { };
  pinnedPkg = builtins.fetchGit {
    name = "nixos-unstable-2019-10-06";
    url = https://github.com/nixos/nixpkgs/;
    rev = "271fef8a4eb03cd9de0c1fe2f0b7f4a16c2de49a";
  };
  npkgs = if nixpkgs == null then
           import pinnedPkg {}
         else
           import nixpkgs {};
  variant = if doBenchmark then npkgs.pkgs.haskell.lib.doBenchmark else npkgs.pkgs.lib.id;
  verismith = npkgs.pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
in
variant verismith
