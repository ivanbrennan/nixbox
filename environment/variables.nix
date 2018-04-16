{
  DOTFILES = "$HOME/Development/resources/dotfiles";
  EDITOR = "vim";
  VISUAL = "vim";
  GIT_EDITOR = "vim";
  GIT_MERGE_AUTOEDIT = "no";
  FZF_DEFAULT_COMMAND = "fd -t file -E GTAGS -E GRTAGS -E GPATH";
  FZF_DEFAULT_OPTS = ''
    --prompt='• '
    --history=$HOME/.fzf_history
    --bind=ctrl-p:up
    --bind=ctrl-n:down
    --bind=alt-p:previous-history
    --bind=alt-n:next-history
    --exact
    --color=fg+:#c4d5e5
    --color=bg+:#222222
    --color=hl+:#f8bb39
    --color=hl:#26a6a6
    --color=info:#48778b
    --color=marker:#bbe068
    --color=pointer:#222222
    --color=prompt:#b8e068
    --color=spinner:#181818
  '';
  FZF_CTRL_T_COMMAND = "$FZF_DEFAULT_COMMAND";
  FZF_CTRL_T_OPTS = ''
    --preview='[[ \$(file --mime {}) =~ binary ]] &&
                   echo {} is a binary file ||
                   (highlight -O ansi -l {} ||
                    cat {}) 2> /dev/null | head -500'
    --bind="?:toggle-preview"
  '';
  FZF_TMUX = "1";
}
