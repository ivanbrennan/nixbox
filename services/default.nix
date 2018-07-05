{
  services = {
    emacs.enable = true;
    gnome3.gpaste.enable = true;
    openvpn = import ./openvpn;
    postgresql.enable = true;
    xserver = import ./xserver.nix;
  };
}
