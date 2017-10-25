# Check for interactive
[[ $- != *i* ]] && return

# Notify of completed background jobs
set -o notify

# Bash options
shopt -s cdspell
shopt -s direxpand
shopt -s dirspell
shopt -s extglob
shopt -s no_empty_cmd_completion

# History configuration
HISTCONTROL=ignoreboth:erasedups
HISTSIZE=20000
HISTFILESIZE=${HISTSIZE}
HISTTIMEFORMAT="%F %T "

# Bash history options
shopt -s histverify
shopt -s cmdhist
shopt -s histappend

# Colored listings
if [[ -r ~/.dircolors ]] && type -p dircolors >/dev/null; then
    eval "$(dircolors -b "$HOME/.dircolors")"
fi

# Load aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Load custom functions
if [ -f ~/.bash_functions ]; then
    . ~/.bash_functions
fi

# Bash completion
if [[ -r "/usr/share/bash-completion/bash_completion" ]]; then
    . "/usr/share/bash-completion/bash_completion"
fi

# Git prompt format
GIT="\$(__git_ps1 ' %s')"

# Prompt window title
TITLE='\[\e]2;\u@\h:\W\a\]'

# Prompt colors
RED='\[\033[1;31m\]'
GREEN='\[\033[1;32m\]'
BLUE='\[\033[1;34m\]'
NIL='\[\033[00m\]'

# Prompt format
case $TERM in
    xterm*|rxvt*|st*|screen*|tmux*)
        PS1="${TITLE}${BLUE}\w${RED}${GIT}${GREEN} > ${NIL}"
        ;;
    *)
        PS1="${BLUE}\w${RED}${GIT}${GREEN} > ${NIL}"
        ;;
esac
