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

in
{ verifuzz = pkgs.haskellPackages.callPackage ./. { };
}
