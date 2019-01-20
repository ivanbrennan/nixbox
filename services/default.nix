{
  services = {
    emacs.enable = true;
    gnome3.gpaste.enable = true;
    openvpn = import ./openvpn;
    timesyncd.enable = false;
    xserver = import ./xserver.nix;
  };
}
