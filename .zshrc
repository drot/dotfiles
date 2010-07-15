# drot zsh

# --- global ---
export PATH="${PATH}:${HOME}/bin"
export EDITOR="emacsclient"
export ALTERNATE_EDITOR="emacs"
export VISUAL="${EDITOR}"
export PAGER="less"
export BROWSER="conkeror"

# --- history ---
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=10000
export SAVEHIST=10000

# --- grep, man and dircolors ---
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32' # beautify grep
export GROFF_NO_SGR=1 # output ANSI color escape sequences in raw form
export LESS_TERMCAP_mb=$'\E[0;31m' # blinking
export LESS_TERMCAP_md=$'\E[1;34m' # bold, used for headings
export LESS_TERMCAP_us=$'\E[1;32m' # underline, used for paths,keywords
export LESS_TERMCAP_so=$'\E[41;1;37m' # standout, used for statusbar/search
export LESS_TERMCAP_ue=$'\E[0m' # end underline
export LESS_TERMCAP_se=$'\E[0m' # end standout-mode
export LESS_TERMCAP_me=$'\E[0m' # end all modes like so, us, mb, md and mr
eval `dircolors -b "${HOME}/.dircolors"` #dircolors

# --- aliases ---
alias ls="ls --color=auto"

# --- zsh settings ---
setopt emacs # emacs keybindings
setopt autocd # lazy dir switching
setopt nohup # don't kill processes
setopt noclobber # don't overwrite files
setopt nobanghist # disable chs-style history
setopt cdablevars #
setopt histreduceblanks histignorespace inc_append_history

# --- key bindings ---
bindkey '^[[A' history-beginning-search-backward # "Up"
bindkey '^[[B' history-beginning-search-forward  # "Down"

# --- completion ---
autoload -U compinit; compinit
#  * list of completers to use
zstyle ":completion:*" completer _complete _match _approximate
#  * allow approximate
zstyle ":completion:*:match:*" original only
zstyle ":completion:*:approximate:*" max-errors 1 numeric
#  * selection prompt as menu
zstyle ":completion:*" menu select=1
#  * menu selection for PID completion
zstyle ":completion:*:*:kill:*" menu yes select
zstyle ":completion:*:kill:*" force-list always
zstyle ":completion:*:processes" command "ps -au$USER"
zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#)*=0=01;32"
#  * don't select parent dir on cd
zstyle ":completion:*:cd:*" ignore-parents parent pwd
#  * complete with colors
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# --- functions ---

# extraction
function extract () {
    if [[ -f "$1" ]]; then
        case "$1" in
            *.tbz2 | *.tar.bz2) tar -xvjf  "$1"     ;;
            *.txz | *.tar.xz)   tar -xvJf  "$1"     ;;
            *.tgz | *.tar.gz)   tar -xvzf  "$1"     ;;
            *.tar | *.cbt)      tar -xvf   "$1"     ;;
            *.zip | *.cbz)      unzip      "$1"     ;;
            *.rar | *.cbr)      unrar x    "$1"     ;;
            *.arj)              unarj x    "$1"     ;;
            *.ace)              unace x    "$1"     ;;
            *.bz2)              bunzip2    "$1"     ;;
            *.xz)               unxz       "$1"     ;;
            *.gz)               gunzip     "$1"     ;;
            *.7z)               7z x       "$1"     ;;
            *.Z)                uncompress "$1"     ;;
            *.gpg)       gpg2 -d "$1" | tar -xvzf - ;;
            *) echo 'Error: failed to extract "$1"' ;;
        esac
    else
        echo 'Error: "$1" is not a valid file for extraction'
    fi
}

# --- window title ---
case $TERM in
    *xterm*|rxvt|rxvt-unicode|rxvt-256color|(dt|k|E)term)
    precmd () { print -Pn "\e]0;$TERM - [%n@%M]%# [%~]\a" } 
    preexec () { print -Pn "\e]0;$TERM - [%n@%M]%# [%~] ($1)\a" }
    ;;
esac

# --- prompt ---
setprompt () {
	# load some modules
	autoload -U colors zsh/terminfo # Used in the colour alias below
	colors
	setopt extended_glob prompt_subst

	# make some aliases for the colours: (coud use normal escap.seq's too)
	for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
		eval PR_$color='%{$fg[${(L)color}]%}'
	done
	PR_NO_COLOR="%{$terminfo[sgr0]%}"

	# Check the UID
	if [[ $UID -ge 1000 ]]; then # normal user
		eval PR_USER='${PR_GREEN}%n${PR_NO_COLOR}'
		eval PR_USER_OP='${PR_GREEN}%#${PR_NO_COLOR}'
	elif [[ $UID -eq 0 ]]; then # root
		eval PR_USER='${PR_RED}%n${PR_NO_COLOR}'
		eval PR_USER_OP='${PR_RED}%#${PR_NO_COLOR}'
	fi	

	# Check if we are on SSH or not
	if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]; then 
		eval PR_HOST='${PR_YELLOW}%M${PR_NO_COLOR}' #SSH
	else 
		eval PR_HOST='${PR_GREEN}%M${PR_NO_COLOR}' # no SSH
	fi
	# set the prompt
	PS1=$'${PR_CYAN}[${PR_USER}${PR_CYAN}@${PR_HOST}${PR_CYAN}][${PR_BLUE}%~${PR_CYAN}]${PR_USER_OP} '
	PS2=$'%_>'
}
setprompt