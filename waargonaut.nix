{ mkDerivation, base, bytestring, criterion, deepseq, digit
, directory, distributive, doctest, filepath, hedgehog, lens, mtl
, nats, parsec, parsers, scientific, semigroups, stdenv, tasty
, tasty-expected-failure, tasty-hedgehog, tasty-hunit
, template-haskell, text
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring deepseq digit distributive lens mtl nats parsers
    scientific semigroups text
  ];
  testHaskellDepends = [
    base bytestring digit directory doctest filepath hedgehog lens
    parsec tasty tasty-expected-failure tasty-hedgehog tasty-hunit
    template-haskell text
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq lens parsec text
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
