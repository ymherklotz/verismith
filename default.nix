{ nixpkgs ? null, compiler ? "ghc865", doBenchmark ? false } :
let
  pinnedPkg = builtins.fetchGit {
    name = "nixos-unstable-2020-03-06";
    url = https://github.com/nixos/nixpkgs/;
    rev = "93ba4ecd58602d3f69f74f9d45d60a8f949544e2";
  };
  npkgs = if nixpkgs == null
          then import pinnedPkg {}
          else import nixpkgs {};
  variant = if doBenchmark then npkgs.pkgs.haskell.lib.doBenchmark else npkgs.pkgs.lib.id;
  verismith = npkgs.pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
in
variant verismith
