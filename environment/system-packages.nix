pkgs:

let
  packages = with pkgs; [
    ag
    chromium
    dmidecode
    docker
    git
    mkpasswd
    par
    slack
    tmux
    tree
    xcape
    xclip
  ];
  vimPackages = (import ./vim/packages.nix) pkgs;
in
  packages ++ vimPackages
