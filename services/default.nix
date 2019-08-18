{
  services = {
    emacs.enable = true;
    openvpn = import ./openvpn;
    xserver = import ./xserver.nix;
  };
}
