pkgs:

let
  packages = with pkgs; [
    abcde
    ag
    alacritty
    aspell
    aspellDicts.en
    bind
    binutils
    cabal2nix
    chromium
    conky
    dmidecode
    docker
    emacs
    flac
    git
    global
    gnumake
    lshw
    lsof
    mkpasswd
    nix-repl
    par
    pstree
    rxvt_unicode
    slack
    sqliteInteractive
    stack
    stow
    tmux
    tree
    universal-ctags
    vlc
    weechat
    xcape
    xsel
  ];
  vimPackages = (import ./vim/packages.nix) pkgs;
in
  packages ++ vimPackages
