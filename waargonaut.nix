{ mkDerivation, base, bytestring, criterion, digit, directory
, distributive, doctest, filepath, hedgehog, lens, mtl, nats
, parsec, parsers, scientific, semigroups, stdenv, tasty
, tasty-expected-failure, tasty-hedgehog, tasty-hunit
, template-haskell, text, weigh, vector
, zippers, transformers, errors, containers, contravariant
, hw-json, hw-prim, hw-balancedparens, hw-rankselect, hw-bits
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit distributive lens mtl nats parsers scientific
    semigroups text zippers transformers errors containers vector contravariant
    hw-json hw-prim hw-balancedparens hw-rankselect hw-bits
  ];
  testHaskellDepends = [
    base bytestring digit directory doctest filepath hedgehog lens
    parsec tasty tasty-expected-failure tasty-hedgehog tasty-hunit
    template-haskell text
  ];
  benchmarkHaskellDepends = [
    base criterion lens parsec text weigh
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
