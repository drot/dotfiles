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

# Set PATH so it includes the user directory
if [ -d "$HOME/.local/bin" ] ; then
    export PATH=$HOME/.local/bin:$PATH
fi

# Initialize Bash
[[ -f ~/.bashrc ]] && . ~/.bashrc
