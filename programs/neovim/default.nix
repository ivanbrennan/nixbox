{ pkgs, ... }:

{
  enable = true;

  configure = {
    customRC = pkgs.neovim-init;

    packages.ncore = with (pkgs.vimPlugins) // (pkgs.vimPrivatePlugins); {
      start =
        [ ncore-plugin
          commentary
          dirvish
          fugitive
          wool
        ];
      opt =
        [ nvim-colorizer-lua
        ];
      # opt =
      #   [ haskell-vim
      #     splitjoin
      #   ];
    };
  };

  runtime = pkgs.neovim-runtime-attrs;
}
