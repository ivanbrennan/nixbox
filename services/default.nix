{ config, pkgs, ... }:

{
  services = {
    emacs.enable = true;

    gvfs.enable = true;

    interception-tools = {
      enable = true;
      udevmonConfig = ./udevmon.yaml;
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
      temperature.night = 4000;
    };

    xbanish.enable = true;

    xserver = (import ./xserver) pkgs;

    picom = {
      enable = true;
      fade = false;
      shadow = false;
      backend = "xrender";
      vSync = true;
    };

    colord.enable = true;
    gnome.gnome-keyring.enable = true;
    hardware.bolt.enable = true;
    udisks2.enable = true;
    upower.enable = config.powerManagement.enable;
  };
}
