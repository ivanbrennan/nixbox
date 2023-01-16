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

  runtime = pkgs.neovim-runtime-attrs;
}
