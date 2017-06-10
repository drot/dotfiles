# Environment variables
export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export VISUAL=${EDITOR}
export PAGER=less
export LESS=-Ri
export GREP_COLORS="mt=01;37;41"
export GIT_PS1_SHOWDIRTYSTATE=yes
export BROWSER=chromium
export NAME="Davor Rotim"
export EMAIL="drot@firemail.cc"
export WINEPREFIX=$HOME/.config/wine/
# export WINEARCH=win32

# Man page colorization
export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\e[01;32m' # begin bold
export LESS_TERMCAP_me=$'\e[0m' # end mode
export LESS_TERMCAP_se=$'\e[0m' # end standout-mode
export LESS_TERMCAP_so=$'\e[1;37;41m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\e[0m' # end underline
export LESS_TERMCAP_us=$'\e[04;34m' # begin underline

# Set PATH so it includes user directories
if [ -d "$HOME/bin" ] ; then
    export PATH=$HOME/bin:$PATH
fi

if [ -d "$HOME/.local/bin" ] ; then
    export PATH=$HOME/.local/bin:$PATH
fi

# Initialize Bash
[[ -f ~/.bashrc ]] && . ~/.bashrc
