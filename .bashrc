# Check for interactive
[[ $- != *i* ]] && return

# Bash options
shopt -s cdspell dirspell histverify cmdhist histappend no_empty_cmd_completion extglob

# History
export HISTCONTROL="ignoreboth:erasedups"
export HISTSIZE=20000
export HISTFILESIZE=${HISTSIZE}
export HISTTIMEFORMAT="%F %T "

# Colored listings
if [[ -r ~/.dircolors ]] && type -p dircolors >/dev/null; then
    eval $(dircolors -b "$HOME/.dircolors")
fi

# Colored man pages
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
        LESS_TERMCAP_md=$'\E[01;32;5;74m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[1;37;42m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[04;34;5;146m' \
        man "$@"
}

# Git prompt
if [[ -f /usr/share/git/git-prompt.sh ]]; then
    . /usr/share/git/git-prompt.sh
else
    __git_ps1() { :; }
fi

GIT="\$(__git_ps1 \" (%s)\")"

# Prompt colors
RED='\[\033[1;31m\]'
GREEN='\[\033[1;32m\]'
BLUE='\[\033[1;34m\]'
NIL='\[\033[00m\]'

# Prompt look
export PS1="${BLUE}\w${RED}${GIT}${CYAN}${GREEN} > ${NIL}"

# Aliases
alias ls="ls -h --group-directories-first --color=auto"
alias grep="grep --color=auto"
alias eckd="emacsclient -e '(kill-emacs)'"
