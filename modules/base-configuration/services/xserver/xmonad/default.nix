{
  enable = true;
  extraPackages = ps: [
    ps.data-default
    ps.xmonad-contrib
  ];
  enableConfiguredRecompile = true;
  config = ./xmonad.hs;
  ghcArgs = [
    "-i${./lib}"
    "-hidir /tmp"
    "-odir /tmp"
  ];
}
