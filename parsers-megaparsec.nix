{ mkDerivation, base, megaparsec, mtl, parsers, stdenv, text }:
mkDerivation {
  pname = "parsers-megaparsec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base megaparsec mtl parsers text ];
  license = stdenv.lib.licenses.bsd3;
}
