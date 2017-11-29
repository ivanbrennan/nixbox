pkgs:

let
  customization = {
    name = "vim";
    vimrcConfig.customRC = import ./vimrc/default.nix;
  };
  vim = pkgs.vim_configurable.customize customization;
in [
  vim
]
