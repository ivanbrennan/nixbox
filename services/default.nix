{ config, pkgs, ... }:

{
  services = {
    emacs.enable = true;

    interception-tools = {
      enable = true;
      udevmonConfig = ./udevmon.yaml;
    };

    locate.enable = true;

    openvpn = import ./openvpn;

    redshift = {
      enable = true;
      temperature.night = 5000;
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
    gnome3.gnome-keyring.enable = true;
    hardware.bolt.enable = true;
    udisks2.enable = true;
    upower.enable = config.powerManagement.enable;
  };
}
