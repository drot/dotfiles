# Global
export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export VISUAL=${EDITOR}
export PAGER=less
export BROWSER=conkeror
export PULSE_LATENCY_MSEC=60
[ -d $HOME/bin ] && export PATH=$HOME/bin:$PATH

# History
export HISTIGNORE="\&:ls:ll:la:cd:fg:bg:exit:clear" # don't append consecutive duplicates of these
export HISTCONTROL=ignoreboth # ingore duplicates and spaces (ignoreboth|ignoredups|ignorespace)
export HISTSIZE=20000 # bash history will save N commands
export HISTFILESIZE=${HISTSIZE} # bash will remember N commands
export HISTTIMEFORMAT="[%Y-%m-%d - %H:%M:%S] "

# Color grep and man pages
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;37;42' # beautify grep
export GROFF_NO_SGR=1 # output ANSI color escape sequences in raw form
export LESS_TERMCAP_mb=$'\E[1;31m' # blinking
export LESS_TERMCAP_md=$'\E[1;32m' # bold, used for headings
export LESS_TERMCAP_us=$'\E[1;34m' # underline, used for paths,keywords
export LESS_TERMCAP_so=$'\E[1;37;42m' # standout, used for statusbar/search
export LESS_TERMCAP_ue=$'\E[0m' # end underline
export LESS_TERMCAP_se=$'\E[0m' # end standout-mode
export LESS_TERMCAP_me=$'\E[0m' # end all modes like so, us, mb, md and mr

# Disable ^s/^q flow control
stty -ixon
stty -ixoff

# Pretty dir listings
[ -e $HOME/.dircolors ] && eval $(dircolors -b $HOME/.dircolors)

# Bash options
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s histappend # append to the history file
shopt -s no_empty_cmd_completion # don't search completions in PATH on an empty line
shopt -s extglob # extended globbing

# Prompt colors
RED='\[\033[1;31m\]'
GREEN='\[\033[1;32m\]'
BLUE='\[\033[1;34m\]'
NIL='\[\033[00m\]'

# Git completion
[ -e /usr/share/git/completion/git-completion.bash ] && source /usr/share/git/completion/git-completion.bash
[ -e /usr/share/git/completion/git-prompt.sh ] && source /usr/share/git/completion/git-prompt.sh
GIT="\$(__git_ps1 \" (%s)\")"

# Prompt look
export PS1="${BLUE}\w${RED}${GIT}${CYAN}${GREEN} > ${NIL}"

# Aliases
alias ls="ls -h --group-directories-first --color=auto"
alias eckd="emacsclient -e '(kill-emacs)'"

# Extract - archive extractor
# Usage: extract <file>
function extract () {
    if [[ -f "$1" ]]; then
        case "$1" in
            *.tbz2 | *.tar.bz2) tar -xvjf "$1" ;;
            *.txz | *.tar.xz) tar -xvJf "$1" ;;
            *.tgz | *.tar.gz) tar -xvzf "$1" ;;
            *.tar | *.cbt) tar -xvf "$1" ;;
            *.zip | *.cbz) unzip "$1" ;;
            *.rar | *.cbr) unrar x "$1" ;;
            *.arj) unarj x "$1" ;;
            *.ace) unace x "$1" ;;
            *.bz2) bunzip2 "$1" ;;
            *.xz) unxz "$1" ;;
            *.gz) gunzip "$1" ;;
            *.7z) 7z x "$1" ;;
            *.Z) uncompress "$1" ;;
            *.gpg) gpg2 -d "$1" | tar -xvzf - ;;
            *) echo "Error: failed to extract $1" ;;
        esac
    else
        echo "Error: $1 is not a valid file for extraction"
    fi
}
