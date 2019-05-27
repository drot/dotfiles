# Check for interactive
[[ $- != *i* ]] && return

# Remap stop key for flow control 
stty stop ^P

# Notify of completed background jobs immediately
set -o notify

# Prevent file overwrite on stdout redirection
set -o noclobber

# Shell behavior options
shopt -s cdspell # correct spelling errors in arguments supplied to cd
shopt -s dirspell # correct spelling errors during tab-completion
shopt -s globstar # turn on recursive globbing
shopt -s no_empty_cmd_completion # disable tab-completion on an empty line

# History format and size
HISTSIZE="20000"
HISTFILESIZE="${HISTSIZE}"
HISTCONTROL="ignoreboth:erasedups"
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
HISTTIMEFORMAT="%F %T "

# History options
shopt -s histverify # allow history replacement editing
shopt -s cmdhist # save multi-line commands as one command

# Bash completion
if [[ -r /etc/profile.d/bash_completion.sh ]]; then
    . /etc/profile.d/bash_completion.sh
fi

# Colored listings
if [[ -r $HOME/.dircolors ]] && type -p dircolors >/dev/null; then
    eval $(dircolors -b "$HOME/.dircolors")
fi

# Load aliases
if [[ -r $HOME/.bash_aliases ]]; then
    . "$HOME/.bash_aliases"
fi

# Load custom functions
if [[ -r $HOME/.bash_functions ]]; then
    . "$HOME/.bash_functions"
fi

# Prompt colors
RED="\[\033[0;31m\]"
GREEN="\[\033[0;32m\]"
BLUE="\[\033[1;34m\]"
NIL="\[\033[00m\]"

# Save history after each command execution
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# Trim deep directory paths
PROMPT_DIRTRIM="2"

# Git prompt format
GIT="\$(__git_ps1 ' %s')"
GIT_PS1_SHOWDIRTYSTATE="yes"

# Prompt window title
TITLE="\[\e]2;\u@\h:\W\a\]"

# Check if we are on a SSH connection
if [[ -n $SSH_CLIENT ]]; then
    SSH_CONN="${RED}@ "
fi

# Prompt format
case $TERM in
    xterm*|st*|screen*|tmux*) PS1="${TITLE}${SSH_CONN}${BLUE}\w${RED}${GIT}${GREEN} > ${NIL}" ;;
    *) PS1="${SSH_CONN}${BLUE}\w${RED}${GIT}${GREEN} > ${NIL}" ;;
esac
