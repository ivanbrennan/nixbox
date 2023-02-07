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
    "etc/ftplugin/asm.vim".source = ./runtime/ftplugin/asm.vim;
    "etc/ftplugin/c.vim".source = ./runtime/ftplugin/c.vim;
    "etc/ftplugin/cpp.vim".source = ./runtime/ftplugin/cpp.vim;
    "etc/ftplugin/help.lua".source = ./runtime/ftplugin/help.lua;
    "etc/indent/c.vim".source = ./runtime/indent/c.vim;
    "etc/indent/cpp.vim".source = ./runtime/indent/cpp.vim;
    "etc/indent/java.vim".source = ./runtime/indent/java.vim;
    "etc/indent/make.vim".source = ./runtime/indent/make.vim;
    "etc/indent/sh.vim".source = ./runtime/indent/sh.vim;
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
          telescope-file-browser-nvim
          telescope-fzf-native-nvim
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
