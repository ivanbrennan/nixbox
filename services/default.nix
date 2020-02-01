{ pkgs, ... }:

{
  services = {
    emacs.enable = true;
    openvpn = import ./openvpn;

    redshift = {
      enable = true;
      temperature.night = 4700;
    };

    xserver = (import ./xserver) pkgs;
  };
}
