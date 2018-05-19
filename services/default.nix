{
  services = {
    emacs.enable = true;
    gnome3.gpaste.enable = true;
    openvpn = import ./openvpn;
    xserver = import ./xserver.nix;
  };
}
