{
  enable = true;
  historyLimit = 5000;
  terminal = "xterm-256color";
  extraTmuxConf = builtins.readFile ./tmux.conf;
}
