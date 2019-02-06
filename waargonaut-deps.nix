self: super:
let
  lib = (import <nixpkgs> {}).lib;

  hw-pkg = p: { owner = "haskell-works"; repo = p; pkg = p; };

  pkgs = import ./nix/fetch-overrides.nix {
    pkgs = [
      (hw-pkg "hw-json")
      (hw-pkg "hw-json-simd")
      (hw-pkg "hw-prim")
      (hw-pkg "hw-rankselect")
      (hw-pkg "hw-bits")
      (hw-pkg "hw-mquery")
    ];
  };

  hw-imports = lib.attrsets.filterAttrs
    (n: _: lib.strings.hasPrefix "hw-" n && n != "hw-mquery")
    pkgs;

  hw-nobenched = hpkgs: lib.attrsets.mapAttrs
    (name: val: self.haskell.lib.dontBenchmark (hpkgs.callCabal2nix name val.src {}))
    hw-imports;

in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper:
    # Include most of the hw-overrides
    (hw-nobenched hself) // {

      # This one is in the hackage store, yey
      hw-parser = self.haskell.lib.dontBenchmark (hself.callHackage "hw-parser" "0.1.0.0" {});

      # This one needs the bounds removed (and the benchmarks removed, because
      # it's too fast for me to worry)
      hw-mquery = self.haskell.lib.doJailbreak (
        self.haskell.lib.dontBenchmark (
          hself.callCabal2nix "hw-mquery" pkgs.hw-mquery.src {}
        )
      );

      sop-core = hself.callHackage "sop-core" "0.4.0.0" {};
      generics-sop = hself.callHackage "generics-sop" "0.4.0.1" {};
    });
  });
}
