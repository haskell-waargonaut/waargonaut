self: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
      hw-json   = self.haskell.lib.dontBenchmark (hself.callHackage "hw-json" "0.9.0.1" {});
      hw-parser = hself.callHackage "hw-parser" "0.1.0.0" {};
    });
  });
}
