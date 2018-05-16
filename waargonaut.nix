{ mkDerivation, base, bytestring, digit, directory, doctest
, filepath, hedgehog, lens, mtl, papa, parsec, parsers, scientific
, separated, stdenv, tasty, nats, tasty-expected-failure
, tasty-hedgehog, template-haskell, text, tasty-hunit
, contravariant, zippers, distributive
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit lens mtl papa parsers scientific separated
    text nats contravariant zippers distributive
  ];
  testHaskellDepends = [
    base directory doctest filepath hedgehog parsec tasty tasty-expected-failure
    tasty-hedgehog template-haskell tasty-hunit
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
