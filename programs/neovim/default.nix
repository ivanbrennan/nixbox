{ pkgs, ... }:

{
  enable = true;

  configure = {
    customRC = pkgs.neovim-init;
    packages = pkgs.nvim-configured-packages;
  };

  runtime = pkgs.neovim-runtime-attrs;
}
