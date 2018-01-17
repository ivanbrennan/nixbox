pkgs:

let
  packages = with pkgs; [
    abcde
    ag
    alacritty
    aspell
    aspellDicts.en
    chromium
    conky
    dmidecode
    docker
    emacs
    flac
    git
    global
    lshw
    mkpasswd
    nix-repl
    par
    pstree
    slack
    sqliteInteractive
    stack
    tmux
    tree
    universal-ctags
    vlc
    weechat
    xcape
    xclip
  ];
  vimPackages = (import ./vim/packages.nix) pkgs;
in
  packages ++ vimPackages
