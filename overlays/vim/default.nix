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

  vim_configurable = super.vim_configurable.overrideAttrs (old: {
    # Make the X Toolkit Intrinsics library (libXt) available during the build
    # so that Vim will compile itself with clipboard support.
    buildInputs = old.buildInputs ++ [ self.xorg.libXt ];
  });

  vim-configured = self.vim_configurable.customize {
    name = "vim";
    vimrcConfig = configured;
  };

  neovim-init = builtins.readFile ./init.vim;

  # TODO: is there a way to incorporate programs.neovim.runtime?
  neovim-lush-unwrapped = super.neovim.override {
    configure = {
      customRC = self.neovim-init;
      packages.ncore = with (super.vimPlugins) // (self.vimPrivatePlugins); {
        start =
          [ ncore-plugin
            lush-nvim-plugin
          ];
      };
    };
  };
  neovim-lush = super.runCommandLocal "neovim-lush" {
    buildInputs = [ super.makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper ${self.neovim-lush-unwrapped}/bin/nvim $out/bin/nvim-lush
  '';
}
