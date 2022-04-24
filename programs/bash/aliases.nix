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
  git-default-branch =
    "${git} rev-parse 2>/dev/null && " +
    "${git} branch --list main master | ${grep} -m1 -oP 'main|master'";
  gpc = "${git} log $(git-default-branch)..HEAD -- | wc -l";
  gpp = "${git} log $(git-default-branch)..HEAD --reverse -p";
  gpr = "${git} log $(git-default-branch)..HEAD --reverse";
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
