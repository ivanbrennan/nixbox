{ pkgs, ... }:

let
  ncoherent = pkgs.vimUtils.buildVimPlugin {
    name = "ncoherent";
    src = ./ncoherent;
  };

in
{
  enable = true;

  configure = {
    customRC = builtins.readFile ./init.vim;

    packages.core = with (pkgs.vimPlugins) // (pkgs.vimPrivatePlugins); {
      start =
        [ coot
          ncoherent
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
