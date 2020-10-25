sources: hlib: hself: hsuper: {
  # haskellworks
  hw-bits = hsuper.callCabal2nix "hw-bits" sources.hw-bits {};
  hw-balancedparens = hlib.dontCheck (
    hsuper.callCabal2nix "hw-balancedparens" sources.hw-balancedparens {}
  );
  hw-rankselect = hsuper.callCabal2nix "hw-rankselect" sources.hw-rankselect {};
  hw-excess = hlib.markUnbroken hsuper.hw-excess;
  hw-rankselect-base = hlib.markUnbroken hsuper.hw-rankselect-base;
  hw-json-standard-cursor = hlib.markUnbroken hsuper.hw-json-standard-cursor;

  # other
  natural = hsuper.callPackage sources.natural {};
  digit = hlib.markUnbroken hsuper.digit;
  generic-lens = hlib.dontCheck hsuper.generic-lens_1_2_0_1;
}
