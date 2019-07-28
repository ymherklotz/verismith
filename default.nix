{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865", doBenchmark ? false } :
let
  haskellPackages = nixpkgs.pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
        hedgehog-fn = haskellPackagesNew.callPackage ./nix/hedgehog-fn.nix {};
        tomland = nixpkgs.pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./nix/tomland.nix {});
        parser-combinators = haskellPackagesNew.callPackage ./nix/parser-combinators.nix {};
        tasty-hedgehog = haskellPackagesNew.callPackage ./nix/tasty-hedgehog.nix {};
      };
  };
  variant = if doBenchmark then nixpkgs.pkgs.haskell.lib.doBenchmark else nixpkgs.pkgs.lib.id;
  verifuzz = haskellPackages.callCabal2nix "verifuzz" (./.) {};
in
  variant verifuzz
