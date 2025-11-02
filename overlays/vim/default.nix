self: super:

let
  configured = {
    packages.core = (import ./core-package.nix) self;
    customRC = builtins.readFile "${self.dotvim}/vimrc";
  };

  neovim-init = builtins.readFile ./init.vim;

  # TODO: This will need to change once we have:
  # https://github.com/NixOS/nixpkgs/pull/221832
  runtime-attr = str: {
    "${str}".source = ./runtime + ("/" + str);
  };
  # NOTE: See also environment.etc."xdg/nvim/after/{ftplugin,plugin,syntax}/"
  neovim-runtime-attrs =
    builtins.foldl' (attrs: x: attrs // runtime-attr x) {} [
      "ftdetect/avsc.lua"
      "ftdetect/bats.lua"
      "ftdetect/geojson.lua"
      "ftdetect/mutt.lua"
      "ftdetect/ssh.lua"
      "ftplugin/asm.lua"
      "ftplugin/c.lua"
      "ftplugin/cpp.lua"
      "ftplugin/dirvish.lua"
      "ftplugin/fugitiveblame.lua"
      "ftplugin/git.lua"
      "ftplugin/haskell.lua"
      "ftplugin/help.lua"
      "ftplugin/mail.lua"
      "ftplugin/netrw.lua"
      "ftplugin/qf.lua"
      "ftplugin/ruby.lua"
      "ftplugin/sh.lua"
      "ftplugin/vim.lua"
      "indent/c.lua"
      "indent/cpp.lua"
      "indent/java.lua"
      "indent/make.lua"
      "indent/sh.lua"
    ];

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
          hydra-nvim
          mini-align
          nvim-treesitter.withAllGrammars
          super-shell-indent
          surround
          telescope-file-browser-nvim
          telescope-fzf-native-nvim
          telescope-nvim
          telescope-undo-nvim
          treesj
          vim-abolish
          vim-eunuch
          vim-nix
          # TODO: vim-repeat ?
          wool
        ];
      # NOTE: To list/load opt plugins, type :packadd <Tab>
      opt =
        [ # haskell-vim
          lush-nvim
          nvim-colorizer-lua
          playground
          shipwright-nvim
          # splitjoin
          vim-projectionist
        ];
    };
  };

  vim-full = super.vim-full.overrideAttrs (old: {
    # Make the X Toolkit Intrinsics library (libXt) available during the build
    # so that Vim will compile itself with clipboard support.
    buildInputs = old.buildInputs ++ [ self.xorg.libXt ];
  });

  vim-configured = self.vim-full.customize {
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
