{ pkgs, ... }:

let
  du = "${pkgs.coreutils}/bin/du";
  find = "${pkgs.findutils}/bin/find";
  git = "${pkgs.git}/bin/git";
  grep = "${pkgs.gnugrep}/bin/grep";
  ls = "${pkgs.coreutils}/bin/ls";
  nix-shell = "${pkgs.nix}/bin/nix-shell";
  tmux = "${pkgs.tmux}/bin/tmux";
  xdg-open = "${pkgs.xdg_utils}/bin/xdg-open";
in

{
  SS = "unset __ETC_BASHRC_SOURCED && . /etc/bashrc";
  du1 = "${du} -h -d 1";
  fgg = "fg '%-'";
  finame = "${find} . -name";
  gpc = "${git} log origin/master.. -- | wc -l";
  gpp = "${git} log origin/master.. --reverse -p";
  gpr = "${git} log origin/master.. --reverse";
  grep = "${grep} --color=auto";
  gst = "${git} status";
  ls = "${ls} --color=tty";
  l = "ls -Alh";
  la = "ls -A";
  ll = "ls -l";
  lsd = "ls -d .[!.]*";
  nirb = "${nix-shell} -p ruby interactive-editor --command irb";
  t = tmux;
  tls = "${tmux} ls";
  xopen = xdg-open;
}
