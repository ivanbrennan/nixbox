{ config, pkgs, ... }:

{
  services = {
    emacs.enable = true;

    gvfs.enable = true;

    interception-tools = {
      enable = true;
      udevmonConfig = ''
        - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc 0.1 | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
          DEVICE:
            EVENTS:
              EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
      '';
    };

    kmonad = {
      # TODO: kmonad config is machine-specific
      enable = false;
      configfile = ./config.kbd;
    };

    locate.enable = true;
    locate.pruneNames = [];

    openvpn = import ./openvpn;

    redshift = {
      enable = true;
      temperature = {
        day = 5250;
        night = 3700;
      };
    };

    xbanish.enable = true;

    xserver = (import ./xserver) pkgs;

    picom = {
      enable = true;
      fade = false;
      shadow = false;
      backend = "xr_glx_hybrid";
      vSync = true;
      settings = {
        glx-no-stencil = true;
        # glx-no-rebind-pixmap = true;
        unredir-if-possible = true;
        # xrender-sync-fence = true;
      };
    };

    colord.enable = true;
    gnome.gnome-keyring.enable = true;
    hardware.bolt.enable = true;
    udisks2.enable = true;
    upower.enable = config.powerManagement.enable;
  };
}
