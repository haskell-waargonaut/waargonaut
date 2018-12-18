{ mkDerivation, attoparsec, base, bifunctors, bytestring, Cabal
, cabal-doctest, containers, contravariant, digit, directory
, distributive, doctest, errors, filepath, generics-sop, hedgehog
, hedgehog-fn, hoist-error, hw-balancedparens, hw-bits, hw-json
, hw-prim, hw-rankselect, lens, mmorph, mtl, nats, natural, parsers
, scientific, semigroupoids, semigroups, stdenv, tagged, tasty
, tasty-expected-failure, tasty-hedgehog, tasty-hunit
, template-haskell, text, transformers, vector, witherable
, wl-pprint-annotated, zippers
}:
mkDerivation {
  pname = "waargonaut";
  version = "0.5.0.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base bifunctors bytestring containers contravariant digit
    distributive errors generics-sop hoist-error hw-balancedparens
    hw-bits hw-json hw-prim hw-rankselect lens mmorph mtl nats natural
    parsers scientific semigroupoids semigroups tagged text
    transformers vector witherable wl-pprint-annotated zippers
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers contravariant digit directory
    distributive doctest filepath generics-sop hedgehog hedgehog-fn
    lens mtl natural scientific semigroupoids semigroups tagged tasty
    tasty-expected-failure tasty-hedgehog tasty-hunit template-haskell
    text vector zippers
  ];
  homepage = "https://github.com/qfpl/waargonaut";
  description = "JSON wrangling";
  license = stdenv.lib.licenses.bsd3;
}
