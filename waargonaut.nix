{ mkDerivation, base, bytestring, digit, directory, doctest
, filepath, lens, mtl, parsec, parsers, QuickCheck, quickcheck-text
, separated, stdenv, template-haskell, text, scientific, papa
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit lens mtl parsers separated text scientific papa
  ];
  testHaskellDepends = [
    base directory doctest filepath parsec QuickCheck quickcheck-text
    template-haskell
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
