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

  vim-configured = (self.vim_configurable.overrideAttrs (old: rec {
    version = "8.1.1549";
    src = super.fetchFromGitHub {
      owner = "vim";
      repo = "vim";
      rev = "v${version}";
      sha256 = "1jvj127ff962d6hb1l2vid8c13ychqc6w22rp1pbrqf7i4b5xwf1";
    };
  })).customize {
    name = "vim";
    vimrcConfig = configured;
  };

  neovim = super.neovim.override {
    configure = configured;
  };
}
