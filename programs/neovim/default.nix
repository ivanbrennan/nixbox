{ pkgs, ... }:

let
  current-system-module = pkgs.runCommandLocal "current-system-module" { } ''
    install -D -m644 ${./options.lua} $out/etc/lua/current-system/options.lua
  '';

in
{
  enable = true;

  configure = {
    customRC = ''
      " It probably makes sense to stick primarily to Vimscript for
      " configuration, and leverage lua for writing plugins. Using lua
      " here just to see whether/how it can be done.
      luafile ${./init.lua}

      lua << LUA
        -- It probably makes more sense to load these options as a package.
        -- This is mostly a proof-of-concept.
        vim.opt.runtimepath:prepend('${current-system-module}/etc')
        require('current-system/options')

        -- load ~/.config/nvim/lua/user/local.lua if it exists
        pcall(require, 'user/local')
      LUA
    '';

    packages.core = with (pkgs.vimPlugins) // (pkgs.vimPrivatePlugins); {
      start =
        [ coot
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
