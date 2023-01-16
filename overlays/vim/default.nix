self: super:

let
  configured = {
    packages.core = (import ./core-package.nix) self;
    customRC = builtins.readFile "${self.dotvim}/vimrc";
  };

  neovim-init = builtins.readFile ./init.vim;

  # TODO: Remove etc/ prefix once we have the following fix:
  # https://github.com/NixOS/nixpkgs/pull/209755
  neovim-runtime-attrs = {
    "etc/ftplugin/c.vim".source = ./runtime/ftplugin/c.vim;
    # "ftplugin/c.vim".text = "setlocal omnifunc=v:lua.vim.lsp.omnifunc";
    # "ftplugin/d.vim".source = ./ftplugin/d.vim;
    # "ftplugin/d.vim".enabled = false;
  };

  # NOTE: Based on nixos/modules/programs/neovim.nix
  neovim-runtime = super.linkFarm "neovim-runtime" (
    super.lib.mapAttrs (name: value: value.source) neovim-runtime-attrs
  );
  neovim-with-lush = super.neovim.override {
    configure = {
      customRC = neovim-init + ''
        set runtimepath^=${neovim-runtime}/etc
      '';
      packages.ncore = with (super.vimPlugins) // (self.vimPrivatePlugins); {
        start =
          [ ncore-plugin
            lush-nvim
            shipwright-nvim
          ];
      };
    };
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

  inherit neovim-init neovim-runtime-attrs;

  neovim-lush = super.runCommandLocal "neovim-lush" {
    buildInputs = [ super.makeWrapper neovim-with-lush ];
  } ''
    makeWrapper ${neovim-with-lush}/bin/nvim $out/bin/nvim-lush
  '';
}
