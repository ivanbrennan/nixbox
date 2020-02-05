{ pkgs, ... }:

{
  services = {
    emacs.enable = true;

    interception-tools = (import ./interception-tools) pkgs;

    openvpn = import ./openvpn;

    redshift = {
      enable = true;
      temperature.night = 4700;
    };

    xserver = (import ./xserver) pkgs;
  };
}
