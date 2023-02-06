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
in

{
  dotvim = super.callPackage ./dotvim.nix { };

  vimPrivatePlugins = (import ./plugins.nix) super;

  neovim-configured-packages = with super.vimPlugins // self.vimPrivatePlugins; {
    ncore = {
      start =
        [ ncore-plugin
          commentary
          dirvish
          fugitive
          nvim-treesitter
          telescope-nvim
          wool
        ];
      # NOTE: To list/load opt plugins, type :packadd <Tab>
      opt =
        [ # haskell-vim
          lush-nvim
          nvim-colorizer-lua
          shipwright-nvim
          # splitjoin
        ];
    };
  };

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

  neovim-qt = super.neovim-qt.override {
    neovim = super.neovim.override {
      configure = {
        customRC = neovim-init + ''
          set runtimepath^=${neovim-runtime}/etc
        '';
        packages = self.neovim-configured-packages;
      };
    };
  };
}
