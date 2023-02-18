pkgs:

{
  DOTFILES = "$HOME/Development/resources/dotfiles";
  EDITOR = "vim";
  GIT_CEILING_DIRECTORIES = "/home";
  GIT_EDITOR = "vim";
  GIT_MERGE_AUTOEDIT = "no";
  VISUAL = "vim";
  RIPGREP_CONFIG_PATH = "/etc/ripgreprc";

  FZF_DEFAULT_COMMAND = "fd -t file -E GTAGS -E GRTAGS -E GPATH";
  FZF_DEFAULT_OPTS = ''
    --prompt='• '
    --history=$HOME/.fzf-history
    --bind=ctrl-p:up
    --bind=ctrl-n:down
    --bind=alt-p:previous-history
    --bind=alt-n:next-history
    --exact
    --color=fg+:#c4d5e5
    --color=bg+:#222222
    --color=hl+:#f7c352
    --color=hl:#26a6a6
    --color=info:#48778b
    --color=marker:#bbe068
    --color=pointer:#222222
    --color=prompt:#b8e068
    --color=spinner:#181818
  '';
  FZF_CTRL_T_COMMAND = "$FZF_DEFAULT_COMMAND";
  FZF_CTRL_T_OPTS = ''
    --preview-window="hidden"
    --bind="ctrl-h:toggle-preview"
    --preview='
      [[ \$(file --mime {}) =~ binary ]] &&
        echo {} is a binary file ||
        ( bat --style=numbers --color=always --line-range :500 {} || cat {}
        ) 2> /dev/null | head -500'
  '';
  FZF_TMUX = "1";

  IRBRC = "${pkgs.etcdots}/etc/irbrc";

  NNN_OPTS = "eAR";
  NNN_COLORS = "#fcfcfc";
  NNN_FCOLORS = "c1ddfdbf006025f7c6d6abc4";
  NNN_FIFO = "/tmp/nnn.fifo";
  NNN_PLUG = "t:preview-tabbed";
}
