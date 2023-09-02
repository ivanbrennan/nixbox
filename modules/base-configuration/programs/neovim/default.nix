{ pkgs, ... }:

{
  enable = true;

  configure = {
    customRC = pkgs.neovim-init;
    packages = pkgs.neovim-configured-packages;
  };

  runtime = pkgs.neovim-runtime-attrs;
}
