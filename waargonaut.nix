{ mkDerivation, base, bytestring, digit, directory, doctest
, filepath, hedgehog, lens, mtl, papa, parsec, parsers, QuickCheck
, quickcheck-text, scientific, separated, stdenv, tasty, nats
, tasty-expected-failure, tasty-hedgehog, template-haskell, text
, trifecta
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit lens mtl papa parsers scientific separated
    text nats trifecta
  ];
  testHaskellDepends = [
    base directory doctest filepath hedgehog parsec QuickCheck
    quickcheck-text tasty tasty-expected-failure tasty-hedgehog
    template-haskell
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
