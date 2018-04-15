pkgs:

let
  core = with (pkgs.vimPlugins // pkgs.vimPrivatePlugins); {
    start =
      [ articulate
        bstack
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
        vim-easy-align
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
      [ splitjoin
        vim-javascript-syntax
        vim-racket
        vim-rails
        vim-rake
        vim-spec-runner
        vim-tmux-runner
        vmux
        wmgraphviz
      ];
  };
  # To automatically load a plugin when opening a filetype, add vimrc lines like:
  # autocmd FileType php :packadd phpCompletion

in
  pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig = {
      packages.core = core;
      customRC = builtins.readFile "${pkgs.dotvim}/vimrc";
    };
  }
