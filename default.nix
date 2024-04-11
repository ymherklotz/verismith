{ nixpkgs ? null, compiler ? "ghc865", doBenchmark ? false } :
let
  pinnedPkg = builtins.fetchGit {
    url = https://github.com/nixos/nixpkgs/;
    rev = "d6b3ddd253c578a7ab98f8011e59990f21dc3932";
  };
  npkgs = if nixpkgs == null
          then import pinnedPkg {}
          else import nixpkgs {};
  variant = if doBenchmark then npkgs.pkgs.haskell.lib.doBenchmark else npkgs.pkgs.lib.id;
  verismith = npkgs.pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
in
variant verismith
