self: super: {
  haskellPackages = super.haskellPackages.override (_: {
    overrides = hself: hsuper:
    let
      dbcc = n: p: h: super.haskell.lib.dontBenchmark (h.callCabal2nix n (import p) {});
      dc = d: super.haskell.lib.dontCheck d;
    in 
    {
      hedgehog = hsuper.callHackageDirect {
        pkg = "hedgehog";
        ver = "1.0";
        sha256 = "06q1w1pjvhdr6za1n5kjd3zszh4xi2ixrwgclqqqj6nhdiz8y6zj";
      } {};

      hw-hedgehog = super.haskell.lib.doJailbreak (hsuper.callHackageDirect {
        pkg = "hw-hedgehog";
        ver = "0.1.0.3";
        sha256 = "01bmlhb3ns3k9sg3i4q2rx5ab49ns7b2mmq81vg4j6dn5y5hcqkr";
      } {});

      hedgehog-fn = hsuper.callHackageDirect {
        pkg = "hedgehog-fn";
        ver = "1.0";
        sha256 = "1dhfyfycy0wakw4j7rr01a7v70yms7dw3h60k5af7pi9v700wyb4";
      } {};

      tasty-hedgehog = hsuper.callHackageDirect {
        pkg = "tasty-hedgehog";
        ver = "1.0.0.1";
        sha256 = "06mffkvscl8r81hjhsvjlyqa843szgv8fays1l9z4jaw2759glsr";
      } {};

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
