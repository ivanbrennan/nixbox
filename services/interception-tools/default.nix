pkgs:

{
  enable = true;
  udevmonConfig = ./udevmon.yaml;
  plugins = with pkgs.interception-tools-plugins; [
    caps2esc
    tab2meta
  ];
}
