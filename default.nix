{ nixpkgs ? null, compiler ? "ghc865", doBenchmark ? false } :
let
  pinnedPkg = builtins.fetchGit {
    url = https://github.com/nixos/nixpkgs/;
    rev = "ee01de29d2f58d56b1be4ae24c24bd91c5380cea";
  };
  npkgs = if nixpkgs == null
          then import pinnedPkg {}
          else import nixpkgs {};
  variant = if doBenchmark then npkgs.pkgs.haskell.lib.doBenchmark else npkgs.pkgs.lib.id;
  verismith = npkgs.pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
in
variant verismith
