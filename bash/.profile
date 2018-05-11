# Environment variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""
export VISUAL="${EDITOR}"
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export BROWSER="firefox"
export NAME="Davor Rotim"
export EMAIL="drot@firemail.cc"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Wine specific variables
export WINEPREFIX="$HOME/.config/wine"
export WINEARCH="win32"
export WINEDEBUG="-all"
# Disable Mono and Gecko install prompt; prevent .desktop creation
export WINEDLLOVERRIDES="winemenubuilder.exe,mscoree,mshtml=d"

# Set PATH so it includes user directory
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi

# Initialize Bash
if [ -n "$BASH" ]; then
    [ -r "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi
