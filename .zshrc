# Modified by drot 

# {{{ User settings

# {{{ Environment
export PATH=$PATH:~/bin
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
export LESSHISTFILE="-"
export PAGER="less"
export VISUAL="vim"
export EDITOR=$VISUAL
export XTERM="urxvtc"
# }}}

# {{{ Dircolors
eval `dircolors -b ~/.dircolors`
# }}}

# {{{ Manual pages
#     - colorize, since man-db fails to do so
export LESS_TERMCAP_mb=$'\E[01;31m'   # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'   # begin bold
export LESS_TERMCAP_me=$'\E[0m'       # end mode
export LESS_TERMCAP_se=$'\E[0m'       # end standout-mode
export LESS_TERMCAP_so=$'\E[1;33;40m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'       # end underline
export LESS_TERMCAP_us=$'\E[1;32m'    # begin underline
# }}}

# {{{ Aliases
alias ls='ls --color'
# }}}

# {{{ Functions
function extract () {
    if [[ -z "$1" ]]; then
        print -P "Usage: extract filename"
        print -P "Extract a given file based on the extension."
    elif [[ -f "$1" ]]; then
        case "$1" in
            *.tbz2 | *.tar.bz2) tar -xvjf  "$1"     ;;
            *.txz | *.tar.xz)   tar -xvJf  "$1"     ;;
            *.tgz | *.tar.gz)   tar -xvzf  "$1"     ;;
            *.tar | *.cbt)      tar -xvf   "$1"     ;;
            *.zip | *.cbz)      unzip      "$1"     ;;
            *.rar | *.cbr)      unrar x    "$1"     ;;
            *.bz2)              bunzip2    "$1"     ;;
            *.xz)               unxz       "$1"     ;;
            *.gz)               gunzip     "$1"     ;;
            *.7z)               7z x       "$1"     ;;
            *.Z)                uncompress "$1"     ;;
            *) echo "Error: failed to extract '$1'" ;;
        esac
    else
        echo "Error: '$1' is not a valid file for extraction"
    fi
}
# }}}

# {{{ ZSH settings
setopt emacs
setopt nohup
setopt autocd
setopt cdablevars
setopt ignoreeof
setopt nobgnice
setopt nobanghist
setopt noclobber
setopt shwordsplit
setopt interactivecomments
setopt autopushd pushdminus pushdsilent pushdtohome
setopt histreduceblanks histignorespace inc_append_history

# Prompt requirements
setopt extended_glob prompt_subst
autoload colors zsh/terminfo

# new style completion system
autoload -U compinit; compinit
# list of completers to use
zstyle ':completion:*' completer _complete _match _approximate
# allow approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
# selection prompt as menu
zstyle ':completion:*' menu select=1
# menuselection for pid completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
# cd don't select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd
# complete with colors
zstyle ':completion:*' list-colors ''
# }}}


# {{{ Prompt settings
function precmd {
    ###
    # terminal width to one less than the actual width for lineup
    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))
    ###
    # truncate the path if it's too long
    PR_FILLBAR=""
    PR_PWDLEN=""
    local promptsize=${#${(%):---(%n@%m:%l)---()--}}
    local pwdsize=${#${(%):-%~}}
    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
	((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
        PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi
}

function preexec () {
    # Screen window titles as currently running programs
    if [[ "$TERM" == "screen-256color" ]]; then
        local CMD=${1[(wr)^(*=*|sudo|-*)]}
        echo -n "\ek$CMD\e\\"
    fi
}

function setprompt () {
    if [[ "$terminfo[colors]" -ge 8 ]]; then
        colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
	(( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"

    ###
    # try to use extended characters to look nicer
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}
    ###
    # set titlebar text on a terminal emulator
    case $TERM in
	rxvt*)
            PR_TITLEBAR=$'%{\e]0;%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
	    ;;
	screen*)
            PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	    ;;
    esac
    ###
    # Linux console  gets simple prompt, the rest have:
    #   - (user@hostname:tty)--($PWD) and an exit code of the last command
    #   - right hand prompt which makes room if the command line grows past it
    #   - PS2 continuation prompt to match PS1 in color
    case $TERM in
        dumb)
            unsetopt zle
            PROMPT='%n@%m:%~%% '
            ;;
        linux)
            PROMPT='$PR_GREEN%n@%m$PR_WHITE:$PR_YELLOW%l$PR_WHITE:$PR_RED%~$PR_YELLOW%%$PR_NO_COLOUR '
            ;;
	*)
            PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_GREEN$PR_SHIFT_IN$PR_ULCORNER$PR_GREEN$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%(!.%SROOT%s.%n)$PR_GREEN@%m$PR_WHITE:$PR_YELLOW%l\
$PR_GREEN)$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_HBAR${(e)PR_FILLBAR}$PR_GREEN$PR_HBAR$PR_SHIFT_OUT(\
$PR_RED%$PR_PWDLEN<...<%~%<<$PR_GREEN)$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_URCORNER$PR_SHIFT_OUT\

$PR_GREEN$PR_SHIFT_IN$PR_LLCORNER$PR_GREEN$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_RED%?$PR_WHITE:)%(!.$PR_RED.$PR_YELLOW)%#$PR_GREEN)$PR_NO_COLOUR '

            RPROMPT=' $PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

            PS2='$PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_YELLOW%_$PR_GREEN)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
            ;;
    esac
}

# Prompt init
setprompt
# }}}
