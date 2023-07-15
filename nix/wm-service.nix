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
  };

  config = mkIf cfg.enable {
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

  };
}
