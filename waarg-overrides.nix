sources: hlib: hself: hsuper: {
  # doctest = hsuper.callCabal2nix "doctest" sources.doctest {};
  # haskellworks
  hw-bits = hsuper.callCabal2nix "hw-bits" sources.hw-bits {};
  hw-balancedparens = hlib.dontCheck (
    hsuper.callCabal2nix "hw-balancedparens" sources.hw-balancedparens {}
  );
  hw-json-simd =hlib.dontCheck (hlib.markUnbroken hsuper.hw-json-simd);
  hw-rankselect = hsuper.callCabal2nix "hw-rankselect" sources.hw-rankselect {};
  hw-excess = hlib.markUnbroken hsuper.hw-excess;
  hw-rankselect-base = hlib.markUnbroken hsuper.hw-rankselect-base;
  hw-json-standard-cursor = hlib.markUnbroken hsuper.hw-json-standard-cursor;

  # other
  natural = hsuper.callPackage sources.natural {};
  digit = hlib.markUnbroken hsuper.digit;
  hoist-error = hsuper.callPackage sources.hs-hoist-error {};

  # newtype = hlib.dontCheck (hlib.doJailbreak hsuper.newtype);
  ChasingBottoms = hlib.doJailbreak hsuper.ChasingBottoms;
  constraints = hsuper.callCabal2nix "constraints" sources.constraints {};
}
