# Check for interactive
[[ $- != *i* ]] && return

# Bash options
shopt -s cdspell
shopt -s dirspell
shopt -s extglob
shopt -s no_empty_cmd_completion
shopt -s histverify
shopt -s cmdhist
shopt -s histappend

# Notify of completed background jobs
set -o notify

# History configuration
HISTCONTROL=ignoreboth:erasedups
HISTSIZE=20000
HISTFILESIZE=${HISTSIZE}
HISTTIMEFORMAT="%F %T "

# Man pages colorization
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
        LESS_TERMCAP_md=$'\E[01;32m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[1;37;42m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[04;34m' \
        man "$@"
}

# Colored listings
if [[ -r ~/.dircolors ]] && type -p dircolors >/dev/null; then
    eval $(dircolors -b "$HOME/.dircolors")
fi

# Load aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Load custom functions
if [ -f ~/.bash_functions ]; then
    . ~/.bash_functions
fi

# Window title
case ${TERM} in
  xterm*|rxvt*|st*|kterm|gnome*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
  screen*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
esac

# Git prompt
if [[ -f /usr/share/git/git-prompt.sh ]]; then
    . /usr/share/git/git-prompt.sh
else
    __git_ps1() { :; }
fi

GIT="\$(__git_ps1 \" (%s)\")"

# Prompt colors
RED='\[\033[1;31m\]'
GREEN='\[\033[1;33m\]'
BLUE='\[\033[1;34m\]'
NIL='\[\033[00m\]'

# Prompt look
PS1="${BLUE}\W${RED}${GIT}${GREEN} \$ ${NIL}"
