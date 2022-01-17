# Provide a nice prompt if the terminal supports it.

_build_prompt() {
    if [ "$TERM" = "dumb" ] && [ -z "$INSIDE_EMACS" ]; then
        return
    fi

    if [ "$TERM" = "xterm" ]; then
        _build_basic_prompt
    else
        _build_color_prompt
    fi
}

_build_basic_prompt() {
    PS1="\h:\u:\w "
}

_build_color_prompt() {
    local green black bold normal line1 line2

    green="\033[0;32m"
    black="\033[0;30m"
    bold="\033[1m"
    grey="\033[1;30m"
    normal="\033[0m"

    line1="╭\[${bold}\]\w\[${normal}\]\$(_git_ps1_)\[${normal}\] \[${black}\]\$? \d \t\[${normal}\]"
    line2="╰(\u\[${grey}\]@\H\[${normal}\])• "

    PS1="\n${line1}\n${line2}"
    PS2=" ❯ "
    PS4=" + "
}

_git_ps1_() {
    # preserve exit code
    local exit=$?

    local green="\033[0;32m"
    local normal="\033[0m"

    # __git_ps1 inserts the current git branch where %s is
    __git_ps1 " (${green}%s${normal})"

    return $exit
}

_build_prompt

unset _build_prompt _build_basic_prompt _build_color_prompt
