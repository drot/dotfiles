#--------------------
# drot bash
#--------------------

# global
export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export VISUAL=${EDITOR}
export PAGER=less
export BROWSER=conkeror
[ -d $HOME/bin ] && export PATH=$HOME/bin:$PATH

# history
export HISTIGNORE="\&:ls:ll:la:cd:fg:bg:exit:clear" # don't append consecutive duplicates of these
export HISTCONTROL=ignoreboth # ingore duplicates and spaces (ignoreboth|ignoredups|ignorespace)
export HISTSIZE=20000 # bash history will save N commands
export HISTFILESIZE=${HISTSIZE} # bash will remember N commands
export HISTTIMEFORMAT="[%Y-%m-%d - %H:%M:%S] "

# color grep and man pages
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;39;41' # beautify grep
export GROFF_NO_SGR=1 # output ANSI color escape sequences in raw form
export LESS_TERMCAP_mb=$'\E[1;31m' # blinking
export LESS_TERMCAP_md=$'\E[1;32m' # bold, used for headings
export LESS_TERMCAP_us=$'\E[1;34m' # underline, used for paths,keywords
export LESS_TERMCAP_so=$'\E[1;39;41m' # standout, used for statusbar/search
export LESS_TERMCAP_ue=$'\E[0m' # end underline
export LESS_TERMCAP_se=$'\E[0m' # end standout-mode
export LESS_TERMCAP_me=$'\E[0m' # end all modes like so, us, mb, md and mr

# disable ^s/^q flow control
stty -ixon
stty -ixoff

# pretty dir listings
[ -e $HOME/.dir_colors ] && eval $(dircolors -b $HOME/.dir_colors)

# bash options
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s histappend # append to the history file
shopt -s no_empty_cmd_completion # don't search completions in PATH on an empty line
shopt -s extglob # extended globbing

# prompt colors
GREEN='\[\033[0;32m\]'
BLUE='\[\033[0;34m\]'
CYAN='\[\033[0;36m\]'
NIL='\[\033[00m\]'

# git completion
[ -e /usr/share/git/completion/git-completion.bash ] && source /usr/share/git/completion/git-completion.bash
[ -e /usr/share/git/completion/git-prompt.sh ] && source /usr/share/git/completion/git-prompt.sh
GIT="\$(__git_ps1 \" (%s)\")"

# prompt look
export PS1="${CYAN}[${GREEN}\u${CYAN}@${GREEN}\h${CYAN}][${BLUE}\w${GREEN}${GIT}${CYAN}]${GREEN}%${NIL} "

# aliases
alias ls="ls -h --group-directories-first --color=auto"
alias eckd="emacsclient -e '(kill-emacs)'"

# extract - archive extractor
# usage: extract <file>
extract() {
    local c e i

    (($#)) || return

    for i; do
        c=''
        e=1
        
        if [[ ! -r $i ]]; then
            echo "$0: file is unreadable: \`$i'" >&2
            continue
        fi

        case $i in
            *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
c='bsdtar xvf';;
*.7z)  c='7z x';;
*.Z)   c='uncompress';;
*.bz2) c='bunzip2';;
*.exe) c='cabextract';;
*.gz)  c='gunzip';;
*.rar) c='unrar x';;
*.xz)  c='unxz';;
*.zip) c='unzip';;
*)     echo "$0: unrecognized file extension: \`$i'" >&2
continue;;
esac

command $c "$i"
e=$?
done

return $e
}
