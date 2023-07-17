{pkgs ? import <nixpkgs> {}}:
let
  haskellPkgs = with pkgs.haskellPackages; [
    cabal-install
    haskell-language-server
    hlint
    ghcid
    ormolu
    implicit-hie
    X11
    pango
    iwlib
  ];

  xorgPkgs = with pkgs.xorg; [
    libX11
    libX11.dev

    libXft
    libXext
    libXrandr
    libXrender
    libXinerama
    libXScrnSaver
    libXdmcp
  ];
in
  pkgs.mkShell {
    buildInputs = haskellPkgs ++ xorgPkgs ++ (with pkgs; [
      pkg-config
      autoconf
      expat
      pango
    ]);
  }
