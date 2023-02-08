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
    "etc/ftdetect/bats.lua".source = ./runtime/ftdetect/bats.lua;
    "etc/ftdetect/geojson.lua".source = ./runtime/ftdetect/geojson.lua;
    "etc/ftdetect/mutt.lua".source = ./runtime/ftdetect/mutt.lua;
    "etc/ftdetect/ssh.lua".source = ./runtime/ftdetect/ssh.lua;
    "etc/ftplugin/asm.lua".source = ./runtime/ftplugin/asm.lua;
    "etc/ftplugin/c.lua".source = ./runtime/ftplugin/c.lua;
    "etc/ftplugin/cpp.lua".source = ./runtime/ftplugin/cpp.lua;
    "etc/ftplugin/help.lua".source = ./runtime/ftplugin/help.lua;
    "etc/indent/c.lua".source = ./runtime/indent/c.lua;
    "etc/indent/cpp.lua".source = ./runtime/indent/cpp.lua;
    "etc/indent/java.lua".source = ./runtime/indent/java.lua;
    "etc/indent/make.lua".source = ./runtime/indent/make.lua;
    "etc/indent/sh.lua".source = ./runtime/indent/sh.lua;
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
