{ mkDerivation, alex, array, base, binary, blaze-html, bytestring
, Cabal, cabal-doctest, cryptonite, deepseq, DRBG, exceptions, fgl
, fgl-visualize, filepath, gitrev, hedgehog, hedgehog-fn, lens
, lifted-base, memory, monad-control, optparse-applicative, parsec
, prettyprinter, random, recursion-schemes, shakespeare, shelly
, stdenv, tasty, tasty-hedgehog, tasty-hunit, template-haskell
, text, time, tomland, transformers, transformers-base
}:
mkDerivation {
  pname = "verifuzz";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    array base binary blaze-html bytestring cryptonite deepseq DRBG
    exceptions fgl fgl-visualize filepath gitrev hedgehog lens
    lifted-base memory monad-control optparse-applicative parsec
    prettyprinter random recursion-schemes shakespeare shelly
    template-haskell text time tomland transformers transformers-base
  ];
  libraryToolDepends = [ alex ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base fgl hedgehog hedgehog-fn lens parsec shakespeare tasty
    tasty-hedgehog tasty-hunit text
  ];
  homepage = "https://github.com/ymherklotz/VeriFuzz#readme";
  description = "Random verilog generation and simulator testing";
  license = stdenv.lib.licenses.bsd3;
}
