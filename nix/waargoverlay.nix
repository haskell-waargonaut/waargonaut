_: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper:
    let
      hlib = super.haskell.lib;
      de-broken = hlib.markUnbroken;
      handle-hw = drv: hlib.dontCheck (hlib.setBuildTarget (de-broken hsuper.${drv}) "lib:${drv}");
    in {
      natural = hlib.dontCheck (de-broken hsuper.natural);
      # weeeeeeeee
      hw-json-simd = handle-hw "hw-json-simd";
      hw-json-standard-cursor = handle-hw "hw-json-standard-cursor";
      hw-rankselect = hlib.dontCheck (hlib.setBuildTargets (de-broken hsuper.hw-rankselect) [
        "lib:hw-rankselect-gen"
        "lib:hw-rankselect"
      ]);
    });
  });
}