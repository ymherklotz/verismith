{ compiler ? "default", doBenchmark ? false }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          hedgehog-fn = haskellPackagesNew.callPackage ./nix/hedgehog-fn.nix {};
          tomland = haskellPackagesNew.callPackage ./nix/tomland.nix {};
          parser-combinators = haskellPackagesNew.callPackage ./nix/parser-combinators.nix {};
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage (import ./.) {});
in
  if pkgs.lib.inNixShell then drv.env else drv
