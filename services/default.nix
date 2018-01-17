{
  services = {
    xserver = import ./xserver.nix;
    emacs.enable = true;
  };
}
