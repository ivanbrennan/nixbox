pkgs:

with (pkgs.vimPlugins) // (pkgs.vimPrivatePlugins); {
  start =
    [ abolish
      articulate
      bstack
      clever-f-vim
      coherent
      commentary
      coot
      dirvish
      edot
      ftglue
      fugitive
      fzf-vim
      fzfWrapper
      gundo
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
      surround
      tabtab
      targets-vim
      traces
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
