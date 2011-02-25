#--------------------
# drot bash
#--------------------

# global
export EDITOR=emacsclient
export ALTERNATE_EDITOR=emacs
export VISUAL=${EDITOR}
export PAGER=less
[ -d $HOME/bin ] && export PATH=$HOME/bin:$PATH

# history
export HISTIGNORE="\&:ls:ll:la:cd:fg:bg:exit:clear" # don't append consecutive duplicates of these
export HISTCONTROL=ignoreboth # ingore duplicates and spaces (ignoreboth|ignoredups|ignorespace)
export HISTSIZE=10000 # bash history will save N commands
export HISTFILESIZE=${HISTSIZE} # bash will remember N commands
export HISTTIMEFORMAT="[%Y-%m-%d - %H:%M:%S] "

# color grep and man pages
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32' # beautify grep
export GROFF_NO_SGR=1 # output ANSI color escape sequences in raw form
export LESS_TERMCAP_mb=$'\E[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m' # begin bold
export LESS_TERMCAP_me=$'\E[0m' # end mode
export LESS_TERMCAP_se=$'\E[0m' # end standout-mode
export LESS_TERMCAP_so=$'\E[1;33;40m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m' # end underline
export LESS_TERMCAP_us=$'\E[1;32m' # begin underline

# disable ^s/^q flow control
stty -ixon
stty -ixoff

# pretty dir listings
[ -e $HOME/.dircolors ] && eval $(dircolors -b $HOME/.dircolors)

# bash options
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s histappend # append to the history file
shopt -s no_empty_cmd_completion # don't search completions in PATH on an empty line

# sudo bash completion and advanced completion
complete -cf sudo
[ -e /etc/bash_completion ] && source /etc/bash_completion

# prompt colors
GREEN='\[\033[0;32m\]'
YELLOW='\[\033[1;33m\]'
NIL='\[\033[00m\]'

[ -e $HOME/.git-completion.sh ] && source $HOME/.git-completion.sh
GITSTATUS="\$(__git_ps1 \" (%s)\")"

# prompt look
PS1="${GREEN}[${YELLOW}\u${GREEN}@${YELLOW}\h${GREEN}]${GREEN}[${YELLOW}\w${GREEN}]${GREEN}${GITSTATUS}${YELLOW} \$${NIL} "

# aliases
alias ls="ls -h --group-directories-first --color=auto"
alias ec="emacsclient -a emacs"
alias eckd="emacsclient -e '(kill-emacs)'"

# functions
# extract - archive extractor
# usage: extract <file>
extract() {    
  if [ -f "$1" ] ; then
    case "$1" in
      *.tar.bz2) tar xvjf "$1"   ;;
      *.tar.gz)  tar xvzf "$1"   ;;
      *.bz2)     bunzip2 "$1"    ;;
      *.rar)     unrar x "$1"    ;;
      *.gz)      gunzip "$1"     ;;
      *.tar)     tar xvf "$1"    ;;
      *.tbz2)    tar xvjf "$1"   ;;
      *.tgz)     tar xvzf "$1"   ;;
      *.zip)     unzip "$1"      ;;
      *.Z)       uncompress "$1" ;;
      *.7z)      7z x "$1"       ;;
      *)
      echo "$1 is not a valid archive"
      return 1
      ;;
    esac
  else
    echo "$1 is not a valid file"
    return 1
  fi
  return 0
}
