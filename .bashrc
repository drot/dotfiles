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

# History
export HISTCONTROL="ignoreboth:erasedups"
export HISTSIZE=20000
export HISTFILESIZE=${HISTSIZE}
export HISTTIMEFORMAT="%F %T "

# Less options
export LESS=-R
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[1;37;42m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;34m'

# Grep options
export GREP_COLORS="mt=01;37;42"

# Colored listings
if [[ -r ~/.dircolors ]] && type -p dircolors >/dev/null; then
    eval $(dircolors -b "$HOME/.dircolors")
fi

# Load aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

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
export PS1="${BLUE}\W${RED}${GIT}${GREEN} \$ ${NIL}"
