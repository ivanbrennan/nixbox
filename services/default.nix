{
  services = {
    emacs.enable = true;
    openvpn = import ./openvpn;
    redshift.enable = true;
    xserver = import ./xserver.nix;
  };
}
