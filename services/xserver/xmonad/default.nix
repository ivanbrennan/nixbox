{
  enable = true;
  extraPackages = ps: [
    ps.data-default
    ps.xmonad-contrib
  ];
  config = ./xmonad.hs;
  enableConfiguredRecompile = true;
}
