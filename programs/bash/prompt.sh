# Provide a nice prompt if the terminal supports it.
if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
  CYAN="\033[0;36m"
  BOLD="\033[1m"
  NORMAL="\033[0m"

  PS1="╭\[${BOLD}\]\w\[${NORMAL}\]\$(_git_ps1_)\[${NORMAL}\]\n╰(\u)• "
  PS2=" ❯ "
  PS4=" + "

  _git_ps1_() {
    if [ -n "$(__gitdir)" ]; then
      local color="${CYAN}"

      # __git_ps1 inserts the current git branch where %s is
      echo " $(__git_ps1 "(${color}%s${NORMAL})")"
    fi
  }

  if test "$TERM" = "xterm"; then
    PS1="\[\033]2;\h:\u:\w\007\]${PS1}"
  fi
fi
