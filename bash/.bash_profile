# Environment variables
export PATH=$HOME/.local/bin:$PATH
export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export VISUAL=${EDITOR}
export PAGER=less
export LESS=-Ri
export GREP_COLORS="mt=01;90;43"
export BROWSER=conkeror
export NAME="Davor Rotim"
export EMAIL="drot@firemail.cc"
export WINEPREFIX=$HOME/.config/wine/
export WINEARCH=win32

# Initialize Bash
[[ -f ~/.bashrc ]] && . ~/.bashrc
