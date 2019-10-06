{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865", doBenchmark ? false } :
let
  variant = if doBenchmark then nixpkgs.pkgs.haskell.lib.doBenchmark else nixpkgs.pkgs.lib.id;
  verismith = nixpkgs.pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
in
variant verismith
