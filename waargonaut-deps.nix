(self: super:
let
  sources = {
    digit          = import ./nix/digit.nix;
    hoist-error    = import ./nix/hoist-error.nix;
    hedgehog       = import ./nix/hedgehog.nix;
    tasty          = import ./nix/tasty.nix;
    tasty-hedgehog = import ./nix/tasty-hedgehog.nix;
  };
in
{
  digit          = self.callCabal2nix "digit" sources.digit {};

  hoist-error    = self.callCabal2nix "hoist-error" sources.hoist-error {};
  tasty-hedgehog = self.callCabal2nix "tasty-hedgehog" sources.tasty-hedgehog {};

  hedgehog       = self.callCabal2nix "hedgehog" "${sources.hedgehog}/hedgehog" {};
  tasty-hunit    = self.callCabal2nix "tasty-hunit" "${sources.tasty}/hunit" {};

  generics-sop   = self.callHackage "generics-sop" "0.3.2.0" {};
})
