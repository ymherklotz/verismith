{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865", doBenchmark ? false } :
let
  haskellPackages = nixpkgs.pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      hedgehog-fn = haskellPackages.callCabal2nix "hedgehog-fn" (builtins.fetchGit {
        url = "git@github.com:qfpl/hedgehog-fn";
        rev = "723b67f54422cf1fbbdcfa23f01a2d4e37b2d110";
      }) {};
      tomland = nixpkgs.pkgs.haskell.lib.dontCheck (haskellPackages.callCabal2nix "tomland" (builtins.fetchGit {
        url = "git@github.com:kowainik/tomland";
        rev = "a3feec3919e7b86275b0d937d48d153a4beda1f8";
      }) {});
      parser-combinators = haskellPackages.callCabal2nix "parser-combinators" (builtins.fetchGit {
        url = "git@github.com:mrkkrp/parser-combinators";
        rev = "7003fd8425c3bba9ea25763173baedb4ebd184fd";
      }) {};
      tasty-hedgehog = haskellPackages.callCabal2nix "tasty-hedgehog" (builtins.fetchGit {
        url = "git@github.com:qfpl/tasty-hedgehog";
        rev = "214f4496afb03630d12d4db606fb8953b3e02d10";
      }) {};
      hedgehog = haskellPackages.callCabal2nix "hedgehog" (builtins.fetchGit {
        url = "git@github.com:hedgehogqa/haskell-hedgehog";
        rev = "38146de29c97c867cff52fb36367ff9a65306d76";
      }) {};
    };
  };
  variant = if doBenchmark then nixpkgs.pkgs.haskell.lib.doBenchmark else nixpkgs.pkgs.lib.id;
  verismith = haskellPackages.callCabal2nix "verismith" (./.) {};
in
  variant verismith
