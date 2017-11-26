{
  enableCompletion = true;
  shellAliases = import ./aliases.nix;
  interactiveShellInit = ''
    CDPATH=".:~/Development:~/Development/resources:~"

    shopt -s histappend cmdhist lithist

    # If shell is interactive, disable START/STOP output control.
    # This allows Ctrl-S to trigger i-search.
    if [[ $- = *i* ]]; then
      # -ixon : Disable START/STOP output control.
      # This allows Ctrl-S to trigger i-search.
      stty -ixon
    fi

    unset -v MAILCHECK

    HISTTIMEFORMAT='%F %T '
    HISTCONTROL=erasedups:ignorespace
    HISTSIZE=10000
    HISTFILESIZE=20000
    HISTIGNORE='jobs:[fb]g:history:gst:tls:fgg'

    shopt -s no_empty_cmd_completion

    complete -F _tmux t

    showpath() {
      col_bold="\033[30;01m"
      col_reset="\033[39;49;00m"

      printf '%bPATH%b:\n%s\n' "$col_bold" "$col_reset" "''${PATH//:/$'\n'}"
    }

    # jump to Bash builtin documentation
    man () {
      case "$(type -t "$1")" in
        builtin)
          local pattern="^ *$1"

          if bashdoc_match "$pattern \+[-[]"; then
            command man bash | less --pattern="$pattern +[-[]"
          elif bashdoc_match "$pattern\b"; then
            command man bash | less --pattern="$pattern[[:>:]]"
          else
            command man bash
          fi
          ;;
        keyword)
          command man bash | less --hilite-search --pattern='^SHELL GRAMMAR$'
          ;;
        *)
          command man "$@"
          ;;
      esac
    }

    bashdoc_match() {
      command man bash | col -b | grep -l "$1" > /dev/null
    }

    fdoc() {
      curl dict://dict.org/d:$1:foldoc
    }
  '';

  promptInit = ''
    # Provide a nice prompt if the terminal supports it.
    if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
      RED="\033[0;31m"
      GREEN="\033[0;32m"
      YELLOW="\033[0;33m"
      CYAN="\033[0;36m"
      WHITE="\033[0;37m"
      BLACK="\033[0;30m"
      NORMAL="\033[0m"

      PS1="╭$CYAN\w$NORMAL\$(grb_git_prompt)$NORMAL\n╰(\u)• "
      PS2=" ❯ "
      PS4=" + "

      grb_git_prompt() {
        if [ -n "$(__gitdir)" ]; then
          local color="$(git_branch_color)"

          # __git_ps1 inserts the current git branch where %s is
          echo " $(__git_ps1 "($color%s$NORMAL)")"
        fi
      }

      git_branch_color() {
        local minutes="$(minutes_since_last_commit)"

        if [ -n "$minutes" ]; then
          echo "$(minutes_color minutes)"
        elif [[ "$THEME" == 'dark' ]]; then
          echo $WHITE
        else
          echo $BLACK
        fi
      }

      minutes_since_last_commit() {
        local now=$(date +%s)
        local last_commit=$(git log --pretty=format:'%at' -1 2>/dev/null)

        if [ "$?" -eq 0 ]; then
          echo $(( (now - last_commit) / 60 ))
        fi
      }

      minutes_color() {
        if [ -z "$1" ]; then
          return
        elif [[ "$1" -gt 30 ]]; then
          echo "$RED"
        elif [[  "$1" -gt 10  ]]; then
          echo "$YELLOW"
        else
          echo "$GREEN"
        fi
      }

      if test "$TERM" = "xterm"; then
        PS1="\[\033]2;\h:\u:\w\007\]$PS1"
      fi
    fi
  '';
}
