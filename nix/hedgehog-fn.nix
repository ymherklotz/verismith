{ mkDerivation, base, contravariant, hedgehog, stdenv, transformers
}:
mkDerivation {
  pname = "hedgehog-fn";
  version = "0.6";
  sha256 = "fb02b67fba97e24c226feba010d2b308934c54e20a0723b6ea7e4eb199f02176";
  revision = "1";
  editedCabalFile = "19v7amg8l6s1gadnya8nxkcbi0vd3wqc7h6gvqvs099qaqm7zbb1";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base contravariant hedgehog transformers
  ];
  homepage = "https://github.com/qfpl/hedgehog-fn";
  description = "Function generation for `hedgehog`";
  license = stdenv.lib.licenses.bsd3;
}
