{inputs, options, config, pkgs, lib, ...}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.wm;
  cfgType = config.type;
  configDir = ../../../config;
in {
  options.modules.desktop.wm = with types; {
    enable = mkBoolOpt false;
    picom = {
      enable = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      nixpkgs.overlays = let
        xm-overlays = with inputs; [xmonad.overlay xmonad-contrib.overlay xmobar.overlay];
      in
        xm-overlays ++ [(import ./overlay.nix)];

      core = {
        xserver = {
          enable = true;
          defaultSession = "none+myxmonad";
          session = {
            name = "myxmonad";
            start = ''
              /usr/bin/env mzanic-xmonad &
              waitPID=$!
            '';
          };
          wmCommand = with pkgs; "${haskellPackages.mzanic-xmonad}/bin/mzanic-xmonad";
        };
        packages = with pkgs; [
          xmonad-log
          haskellPackages.mzanic-xmonad
          haskellPackages.mzanic-xmobar
          trayer
          i3lock
          xss-lock
          networkmanagerapplet
          pasystray
          pavucontrol
          pamixer
          brightnessctl
          maim
          lxappearance
        ];

        fonts = with pkgs; [
          font-awesome_5
          nerdfonts
          cantarell-fonts
          noto-fonts-emoji
        ];
      };

      home.configFile = {
        "xmobar/xpm".source = "${configDir}/xmobar/xpm";
        "xmobar/trayer-padding-icon.sh" = {
          source = "${configDir}/xmobar/trayer-padding-icon.sh";
          executable = true;
        };
        "x11".source = "${configDir}/x11";
        "xmonad/screenshot" = {
          source = "${configDir}/xmonad/takeScreenshot.sh";
          executable = true;
        };
      };
    }
    (mkIf cfg.picom.enable {
      services.picom = {
        enable = true;
        backend = "xrender";
        vSync = true;
        inactiveOpacity = 0.8;
        activeOpacity = 1;
      };
    })
  ]);
}
