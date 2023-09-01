{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:hercules-ci/gitignore.nix/master";
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
      inputs.git-ignore-nix.follows = "git-ignore-nix";
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
    overlay = import ./overlay.nix;
    overlays = [xmonad.overlay xmonad-contrib.overlay xmobar.overlay overlay];
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowBroken = true;
      };
    in rec {
      devShell = import ./nix/shell.nix;
      defaultPackage = pkgs.haskellPackages.mzanic-xmonad;
      module = import ./nix/module.nix;
    }) // {
      inherit overlays overlay;
    };
}
