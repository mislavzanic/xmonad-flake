{options, config, pkgs, lib, ...}:
with lib;
let
  cfg = config.wm.xmonad;
in {
  options.wm.xmonad = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
    xmobar = mkOption {
      default = true;
      type = types.bool;
      example = false;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      environment.systemPackages = with pkgs; [
        xmonad-log
        haskellPackages.mzanic-xmonad
        trayer
        i3lock
      ];

      fonts.fonts = with pkgs; [
        font-awesome_5
        nerdfonts
        cantarell-fonts
        noto-fonts-emoji
      ];

      services = {
        xserver = {
          enable = true;
          displayManager = {
            defaultSession = "none+myxmonad";
          };

          windowManager = {
            session = [
              {
                name = "myxmonad";
                start = ''
                  /usr/bin/env mzanic-xmonad &
                  waitPID=$!
                '';
              }
            ];
          };
        };
      };
    }
    (mkIf cfg.xmobar {
      environment.systemPackages = with pkgs; [
        haskellPackages.mzanic-xmobar
      ];

      environment.etc = {
        "xmobar/xpm".source = ../config/xpm;
        "xmobar/trayer-padding-icon.sh" = {
          source = ../config/trayer-padding-icon.sh;
          mode = "755";
        };
      };
    })
  ]);
}
