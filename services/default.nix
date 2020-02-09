{ pkgs, ... }:

{
  services = {
    emacs.enable = true;

    interception-tools = {
      enable = true;
      udevmonConfig = ./udevmon.yaml;
    };

    openvpn = import ./openvpn;

    redshift = {
      enable = true;
      temperature.night = 4700;
    };

    xbanish.enable = true;

    xserver = (import ./xserver) pkgs;
  };
}
