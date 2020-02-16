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

    compton = {
      enable = true;
      backend = "glx";
      vSync = true;
      settings = {
        glx-no-stencil = true;
        paint-on-overlay = true;
      };
    };
  };
}
