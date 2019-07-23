let
  composeExtensionsList =
    pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});
  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = name: {
        inherit name;
        value = function haskellPackagesOld.${name};
      };
      
    in
      builtins.listToAttrs (map toPackage names);
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = composeExtensionsList [
          (haskellPackagesNew: haskellPackagesOld: rec {
            hedgehog-fn = haskellPackagesNew.callPackage ./nix/hedgehog-fn.nix {};
            tomland = haskellPackagesNew.callPackage ./nix/tomland.nix {};
            parser-combinators = haskellPackagesNew.callPackage ./nix/parser-combinators.nix {};
            tasty-hedgehog = haskellPackagesNew.callPackage ./nix/tasty-hedgehog.nix {};
          })
          (makeOverrides pkgs.haskell.lib.dontCheck ["tomland"])
        ];
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

in
{ verifuzz = pkgs.haskellPackages.callPackage ./. { };
}
