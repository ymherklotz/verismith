{
  description = "A flake for the Haskell Verismith project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        packages = {
          verismith = pkgs.haskellPackages.callCabal2nix "verismith" (./.) {};
        };
        defaultPackage = self.packages.${system}.verismith;
        devShells.default = self.packages.${system}.verismith.env;
      }
    );
}
