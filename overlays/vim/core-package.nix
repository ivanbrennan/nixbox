pkgs:

with (pkgs.vimPlugins) // (pkgs.vimPrivatePlugins); {
  start =
    [ articulate
      bstack
      coherent
      vim-commentary
      coot
      dirvish
      edot
      ftglue
      vim-fugitive
      fzf-vim
      fzf-wrapper
      gundo-vim
      hint
      iota
      latitude
      listical
      mline
      ocursor
      optcycle
      refract
      sohi
      super-shell-indent
      vim-surround
      tabtab
      targets-vim
      traces
      vim-abolish
      vim-easy-align
      vim-elixir
      vim-eunuch
      vim-grepper
      vim-matchit
      vim-nix
      vim-projectionist
      vim-repeat
      vim-ruby
      vim-unimpaired
      vmacs
      zoo
    ];

  opt =
    [ haskell-vim
      splitjoin
      vim-javascript-syntax
      vim-racket
      vim-rails
      vim-rake
      vim-spec-runner
      vim-tmux-runner
      vmux
      wmgraphviz
    ];
}
