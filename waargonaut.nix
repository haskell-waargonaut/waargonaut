{ mkDerivation, base, bytestring, digit, directory, doctest
, filepath, hedgehog, lens, mtl, parsec, parsers, scientific
, stdenv, tasty, tasty-expected-failure, nats
, tasty-hedgehog, template-haskell, text, tasty-hunit
, distributive
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit lens mtl parsers scientific text
    distributive nats
  ];
  testHaskellDepends = [
    base directory doctest filepath hedgehog parsec tasty tasty-expected-failure
    tasty-hedgehog template-haskell tasty-hunit digit
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
