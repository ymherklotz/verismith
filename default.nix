{ nixpkgs ? null, compiler ? "ghc865", doBenchmark ? false } :
let
  pinnedPkg = <nixpkgs>;
  npkgs = if nixpkgs == null
          then import pinnedPkg {}
          else import nixpkgs {};
  variant = if doBenchmark then npkgs.pkgs.haskell.lib.doBenchmark else npkgs.pkgs.lib.id;
  verismith = npkgs.pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
in
variant verismith
