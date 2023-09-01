_: oldPkgs: rec {
  haskellPackages = oldPkgs.haskellPackages.override (old: {
    overrides =
    oldPkgs.lib.composeExtensions (old.overrides or (_: _: {}))
    (self: super: rec {
      mzanic-xmonad = self.callCabal2nix "mzanic-xmonad" ../xmonad {};
      mzanic-xmobar = self.callCabal2nix "mzanic-xmobar" ../xmobar {};
    });
  });
}

