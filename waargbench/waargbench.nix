{ mkDerivation, attoparsec, base, bytestring, criterion, digit
, generics-sop, hedgehog, hw-json, lens, scientific, semigroups
, stdenv, text, waargonaut
}:
mkDerivation {
  pname = "waargbench";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = false;
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion digit generics-sop hedgehog
    hw-json lens scientific semigroups text waargonaut
  ];
  license = stdenv.lib.licenses.bsd3;
}
