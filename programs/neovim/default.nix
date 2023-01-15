{ pkgs, ... }:

{
  enable = true;

  configure = {
    customRC = pkgs.neovim-init;

    packages.ncore = with (pkgs.vimPlugins) // (pkgs.vimPrivatePlugins); {
      start =
        [ ncore-plugin
        ];
      # opt =
      #   [ haskell-vim
      #     splitjoin
      #   ];
    };
  };

  # TODO: Remove etc/ prefix once we have the following fix:
  # https://github.com/NixOS/nixpkgs/pull/209755
  runtime = {
    "etc/ftplugin/c.vim".source = ./runtime/ftplugin/c.vim;
    # "ftplugin/c.vim".text = "setlocal omnifunc=v:lua.vim.lsp.omnifunc";
    # "ftplugin/d.vim".source = ./ftplugin/d.vim;
    # "ftplugin/d.vim".enabled = false;
  };
}
