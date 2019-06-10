self: super: {
  haskellPackages = super.haskellPackages.override (_: {
    overrides = hself: hsuper:
    let
      dbcc = n: p: h: super.haskell.lib.dontBenchmark (h.callCabal2nix n (import p) {});
      dc = d: super.haskell.lib.dontCheck d;
    in 
    {

      hw-mquery     = dc (dbcc "hw-mquery" ./nix/hw-mquery.nix hself);
      hw-bits       = dc (dbcc "hw-bits" ./nix/hw-bits.nix hself);
      hw-simd       = dc (dbcc "hw-simd" ./nix/hw-simd.nix hself);
      hw-rankselect = dc (dbcc "hw-rankselect" ./nix/hw-rankselect.nix hself);
      hw-prim       = dc (dbcc "hw-prim" ./nix/hw-prim.nix hself);
      hw-json-simd  = dc (dbcc "hw-json-simd" ./nix/hw-json-simd.nix hself);
      hw-json       = dc (dbcc "hw-json" ./nix/hw-json.nix hself);

      hw-parser    = hsuper.callHackage "hw-parser" "0.1.0.0" {};
      sop-core     = hsuper.callHackage "sop-core" "0.4.0.0" {};
      generics-sop = hsuper.callHackage "generics-sop" "0.4.0.1" {};

      natural            = dc hsuper.natural;
      hw-balancedparens  = dc hsuper.hw-balancedparens;
      hw-rankselect-base = dc hsuper.hw-rankselect-base;
      hw-excess          = dc hsuper.hw-excess;
    };
  });
}
