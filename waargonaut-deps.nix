self: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
    (import ./waarg-hackage-overrides.nix super.haskell.lib)
  });
}
