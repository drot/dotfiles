# Check for interactive
[[ $- != *i* ]] && return

# Notify of completed background jobs immediately
set -o notify

# Shell behavior options
shopt -s cdspell
shopt -s direxpand
shopt -s dirspell
shopt -s extglob
shopt -s globstar
shopt -s no_empty_cmd_completion

# History format and size
HISTCONTROL="ignoreboth:erasedups"
HISTSIZE="20000"
HISTFILESIZE="${HISTSIZE}"
HISTTIMEFORMAT="%F %T "

# History options
shopt -s histverify
shopt -s cmdhist
shopt -s histappend

# Colored listings
if [[ -r "$HOME/.dircolors" ]] && type -p dircolors >/dev/null; then
    eval "$(dircolors -b "$HOME/.dircolors")"
fi

# Load aliases
if [[ -r "$HOME/.bash_aliases" ]]; then
    . "$HOME/.bash_aliases"
fi

# Load custom functions
if [[ -r "$HOME/.bash_functions" ]]; then
    . "$HOME/.bash_functions"
fi

# Bash completion
if [[ -r /usr/share/bash-completion/bash_completion ]]; then
    . /usr/share/bash-completion/bash_completion
fi

# Trim deep directory paths
PROMPT_DIRTRIM=2

# Git prompt format
GIT="\$(__git_ps1 ' %s')"
GIT_PS1_SHOWDIRTYSTATE="yes"

# Prompt window title
TITLE="\[\e]2;\u@\h:\W\a\]"

# Prompt colors
RED="\[\033[1;31m\]"
GREEN="\[\033[1;32m\]"
BLUE="\[\033[1;34m\]"
NIL="\[\033[00m\]"

# Prompt format
case $TERM in
    xterm*|rxvt*|st*|screen*|tmux*)
        PS1="${TITLE}${BLUE}\w${RED}${GIT}${GREEN} > ${NIL}"
        ;;
    *)
        PS1="${BLUE}\w${RED}${GIT}${GREEN} > ${NIL}"
        ;;
esac
