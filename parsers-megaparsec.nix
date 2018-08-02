{ mkDerivation, base, fail, megaparsec, mtl, parsers, semigroups
, stdenv, text, transformers
}:
mkDerivation {
  pname = "parsers-megaparsec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base fail megaparsec mtl parsers semigroups text transformers
  ];
  description = "`parsers` instances for Megaparsec";
  license = stdenv.lib.licenses.bsd3;
}
