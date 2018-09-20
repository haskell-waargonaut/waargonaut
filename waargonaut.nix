{ mkDerivation, base, bifunctors, bytestring, Cabal, cabal-doctest
, containers, contravariant, digit, directory, distributive
, doctest, errors, filepath, gauge, generics-sop, hedgehog
, hoist-error, lens, mtl, nats, parsec, parsers, scientific
, semigroups, stdenv, tasty, tasty-expected-failure, tasty-hedgehog
, tasty-hunit, template-haskell, text, transformers, vector
, witherable, wl-pprint-annotated, zippers
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.1.0.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base bifunctors bytestring containers contravariant digit
    distributive errors generics-sop hoist-error lens mtl nats parsers
    scientific semigroups text transformers vector witherable
    wl-pprint-annotated zippers
  ];
  testHaskellDepends = [
    base bytestring digit directory distributive doctest filepath
    generics-sop hedgehog lens parsec semigroups tasty
    tasty-expected-failure tasty-hedgehog tasty-hunit template-haskell
    text vector zippers
  ];
  benchmarkHaskellDepends = [
    base gauge lens parsec semigroups text
  ];
  homepage = "https://github.com/qfpl/waargonaut";
  description = "JSON wrangling";
  license = stdenv.lib.licenses.bsd3;
}
