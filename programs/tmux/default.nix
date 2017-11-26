{
  enable = true;
  historyLimit = 5000;
  shortcut = "a";
  terminal = "xterm-256color";
  extraTmuxConf = builtins.readFile ./tmux.conf;
}
