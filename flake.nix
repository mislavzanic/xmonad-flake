{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmobar = {
      url = "git+https://codeberg.org/xmobar/xmobar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    flake-utils,
    nixpkgs,
    xmonad,
    xmonad-contrib,
    xmobar,
    ...
  }: let
    overlay = newPkgs: oldPkgs: rec {
      haskellPackages = oldPkgs.haskellPackages.override (old: {
        overrides =
        oldPkgs.lib.composeExtensions (old.overrides or (_: _: {}))
        (self: super: rec {
          mzanic-xmonad = self.callCabal2nix "mzanic-xmonad" ./xmonad {};
          mzanic-xmobar = self.callCabal2nix "mzanic-xmobar" ./xmobar {};
        });
      });
    };

    overlays = [xmonad.overlay xmonad-contrib.overlay xmobar.overlay overlay];
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowBroken = true;
      };
    in rec {
      devShell = import ./shell.nix;
      wm = import ./nix/wm-service.nix;
      defaultPackage = pkgs.haskellPackages.mzanic-xmonad;
    }) // {
      inherit overlays overlay;
    };
}
