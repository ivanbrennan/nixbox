self: super:

let
  configured = {
    packages.core = (import ./core-package.nix) self;
    customRC = builtins.readFile "${self.dotvim}/vimrc";
  };

in

{
  dotvim = super.callPackage ./dotvim.nix { };

  vimPrivatePlugins = (import ./plugins.nix) super;

  vim-configured = self.vim_configurable.customize {
    name = "vim";
    vimrcConfig = configured;
  };

  neovim = super.neovim.override {
    configure = configured;
  };
}
