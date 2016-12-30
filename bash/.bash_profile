# Environment variables
export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export VISUAL=${EDITOR}
export PAGER=less
export LESS=-Ri
export GREP_COLORS="mt=01;90;43"
export BROWSER=firefox
export NAME="Davor Rotim"
export EMAIL="drot@firemail.cc"
export WINEPREFIX=$HOME/.config/wine/

# Set PATH so it includes private bin if it exists
if [ -d $HOME/.local/bin ] ; then
    export PATH=$HOME/.local/bin:$PATH
fi

# Initialize Bash
[[ -f ~/.bashrc ]] && . ~/.bashrc
