pkgs:

let
  public = with pkgs.vimPlugins; {
    start =
      [ commentary
        fugitive
        fzf-vim
        fzfWrapper
        gundo
        surround
        targets-vim
        vim-easy-align
        vim-eunuch
        vim-nix
        vim-repeat
        vim-ruby
      ];

    opt = [];
  };

  private = with (import ./plugins.nix) pkgs; {
    start =
      [ articulate
        bstack
        coherent
        coot
        dirvish
        edot
        ftglue
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
        tabtab
        vim-grepper
        vim-matchit
        vim-projectionist
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

  vim = pkgs.vim_configurable.customize {
    name = "vim";

    vimrcConfig.packages.core = {
      start = public.start ++ private.start;
      opt = public.opt ++ private.opt;
      # To automatically load a plugin when opening a filetype, add vimrc lines like:
      # autocmd FileType php :packadd phpCompletion
    };

    vimrcConfig.customRC = builtins.readFile "${pkgs.dotvim}/vimrc";
  };
in [ vim ]
