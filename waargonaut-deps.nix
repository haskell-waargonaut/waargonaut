self: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: 
    let
      dbcc = n: p: super.haskell.lib.dontBenchmark (hsuper.callCabal2nix n (import p) {});
    in 
    {
      hw-mquery = super.haskell.lib.dontCheck (dbcc "hw-mquery" ./nix/hw-mquery.nix);

      hw-bits = dbcc "hw-bits" ./nix/hw-bits.nix;
      hw-simd = dbcc "hw-simd" ./nix/hw-simd.nix;
      hw-rankselect = dbcc "hw-rankselect" ./nix/hw-rankselect.nix;
      hw-prim = dbcc "hw-prim" ./nix/hw-prim.nix;
      hw-json-simd = dbcc "hw-json-simd" ./nix/hw-json-simd.nix;
      hw-json = dbcc "hw-json" ./nix/hw-json.nix;

      generic-lens = dbcc "generic-lens" ./nix/generic-lens.nix;

      # Can't build deps without this, but this triggers infinite recursion. Halp...
      lens = hsuper.callHackage "lens" "4.17.1" {};

      hw-parser = hsuper.callHackage "hw-parser" "0.1.0.0" {};
      sop-core = hsuper.callHackage "sop-core" "0.4.0.0" {};
      generics-sop = hsuper.callHackage "generics-sop" "0.4.0.1" {};
    });
  });
}
