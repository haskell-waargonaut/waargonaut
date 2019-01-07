self: super:
let
  sources = {
    digit          = import ./nix/digit.nix;
    hoist-error    = import ./nix/hoist-error.nix;
    hedgehog       = import ./nix/hedgehog.nix;
    tasty-hedgehog = import ./nix/tasty-hedgehog.nix;
    hedgehog-fn    = import ./nix/hedgehog-fn.nix;
    hw-json        = import ./nix/hw-json.nix;
  };
in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
      digit           = hself.callCabal2nix "digit"          sources.digit                  {};
      hoist-error     = hself.callCabal2nix "hoist-error"    sources.hoist-error            {};
      tasty-hedgehog  = hself.callCabal2nix "tasty-hedgehog" sources.tasty-hedgehog         {};
      hedgehog-fn     = hself.callCabal2nix "hedgehog-fn"    sources.hedgehog-fn            {};
      hedgehog        = hself.callCabal2nix "hedgehog"       "${sources.hedgehog}/hedgehog" {};
      generics-sop    = hself.callHackage   "generics-sop"   "0.3.2.0"                      {};
      hw-parser       = hself.callHackage   "hw-parser"      "0.1.0.0"                      {};
      hw-json         = hself.callCabal2nix "hw-json"        sources.hw-json                {};
    });
  });
}
