pkgs:

let
  customization = {
    name = "vim";
    vimrcConfig.customRC = import ./vimrc;
  };
  vim = pkgs.vim_configurable.customize customization;
in [
  vim
]
