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
    fd
    flac
    fzf
    gimp
    git
    global
    gnumake
    gnupg
    lshw
    lsof
    mkpasswd
    nice-backgrounds
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
    vln
    weechat
    xcape
    xsel
  ];
  vimPackages = (import ./vim/packages.nix) pkgs;
in
  packages ++ vimPackages
