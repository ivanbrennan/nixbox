{
  services = {
    xserver = import ./xserver.nix;
    emacs.enable = true;
    gnome3.gpaste.enable = true;
  };
}
