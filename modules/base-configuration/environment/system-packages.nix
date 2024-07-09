pkgs:

with pkgs; [
  _1password
  abcde
  age # TODO: pa, passage
  silver-searcher
  alacritty-wrapped
  arc-icon-theme
  arp-scan
  aspell
  aspellDicts.en
  aws-iam-authenticator
  awscli
  bat
  bats
  bc
  bind
  binutils
  bleep
  cabal-install
  cabal2nix
  cachix
  capitaine-cursors
  cdparanoia
  channel-search
  chromium
  cmus
  computables
  conky
  confluent-cli
  direnv
  diss
  dmenu
  dmenu_cdpath
  dmenu_diss
  dmenu_fontpreview
  dmenu_pass_otp
  dmidecode
  docker
  docker-compose
  dotinit
  dunst
  easyrsa
  emacs
  emacseverywhere
  emc
  exfat
  fd
  feh
  ffmpeg
  file
  flac
  flaccurate
  fly
  fzf
  gimp
  git
  gitAndTools.delta
  git-annex
  glances
  global
  adwaita-icon-theme
  gnumake
  gnupg
  gpick
  graphviz
  haskellPackages.hasktags
  htop
  i3lock
  imagemagick
  inkscape
  jq
  kitty
  kubectl
  kubernetes-helm
  libnotify
  libqalculate
  lshw
  lsof
  lsscsi
  lua51Packages.luacheck
  minikube
  mkcue
  mkpasswd
  mutt
  networkmanager_dmenu
  nice-backgrounds
  nice-icons
  nix-du
  nix-prefetch-git
  nixfmt-classic
  neovim-qt
  nnn
  (nsxiv.override { conf = builtins.readFile nsxiv/config.h; })
  openssl
  openvpn_dmenu
  ormolu
  pandoc
  (pass.withExtensions (exts: [exts.pass-otp]))
  par
  parallel
  pick-one-color
  procs
  psmisc
  pup
  qalculate-gtk
  qbittorrent
  qrencode
  redis
  reptyr
  resound
  ripgrep
  # rover # FIXME
  rxvt_unicode-with-plugins
  screencast
  screenshot
  shared-mime-info
  shellcheck
  slack
  socat
  sqlite-interactive
  st
  stack
  stow
  tabbed
  tcpdump
  tmux
  trayer
  trayer-padding-icon
  tree
  udisks_dmenu
  universal-ctags
  unzip
  urxvt_perls
  vim-configured
  vimPlugins.haskell-vim
  virtualboxHeadless
  vlc
  vln
  weechat
  wget
  wmctrl
  xclip
  xcolor
  xdg-user-dirs
  xdotool
  xmobar
  xorg.xmessage
  xorg.xprop
  xorg.xwininfo
  xsel
  yq
  zathura
  zbar
  zellij
  zip
]
