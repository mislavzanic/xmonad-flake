# XMonad Flake
This is a flake with my XMonad config (previously located [here](https://github.com/mislavzanic/nixos-dotfiles)). 

# Disclamer
This config is tailor-made for my use. Feel free to clone it and use it, but don't expect to "work out of the box" for you.

# How To

This flake exposes an overlay and a module.
The overlay overrides the `haskellPackages.xmonad` package with this one built by cabal.
The module adds some packages I need for this xmonad build and sets up the window manager part.
