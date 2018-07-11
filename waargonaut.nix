{ mkDerivation, base, bytestring, containers, criterion, digit
, directory, distributive, doctest, errors, filepath, hedgehog
, hoist-error, hw-balancedparens, hw-bits, hw-json, hw-prim
, hw-rankselect, lens, mtl, nats, parsec, parsers, scientific
, semigroups, stdenv, tasty, tasty-expected-failure, tasty-hedgehog
, tasty-hunit, template-haskell, text, transformers, vector, weigh
, zippers, witherable, contravariant, wl-pprint-annotated
, algebraic-graphs
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers digit distributive errors hoist-error
    hw-balancedparens hw-bits hw-json hw-prim hw-rankselect lens mtl
    nats parsers scientific semigroups text transformers vector zippers
    witherable contravariant wl-pprint-annotated
    algebraic-graphs
  ];
  testHaskellDepends = [
    base bytestring digit directory doctest filepath hedgehog lens
    parsec semigroups tasty tasty-expected-failure tasty-hedgehog
    tasty-hunit template-haskell text vector containers
  ];
  benchmarkHaskellDepends = [
    base criterion lens parsec text weigh
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
