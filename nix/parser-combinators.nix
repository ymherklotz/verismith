{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "parser-combinators";
  version = "1.1.0";
  sha256 = "ac7642972b18a47c575d2bcd0b2f6c34f33ca2ed3adb28034420d09ced823e91";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mrkkrp/parser-combinators";
  description = "Lightweight package providing commonly useful parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
