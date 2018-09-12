# Environment variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""
export VISUAL="${EDITOR}"
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export BROWSER="qutebrowser"
export NAME="Davor Rotim"
export EMAIL="drot@firemail.cc"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Qt5 default style
export QT_QPA_PLATFORMTHEME="qt5ct"

# TeXdoc specific
export PDFVIEWER_texdoc="llpp"
export DVIVIEWER_texdoc="${PDFVIEWER_texdoc}"

# Wine specific variables
export WINEPREFIX="$HOME/.config/wine"
export WINEARCH="win32"
export WINEDEBUG="-all"
# Disable Mono and Gecko install prompt; prevent .desktop creation
export WINEDLLOVERRIDES="winemenubuilder.exe,mscoree,mshtml=d"

# Check for Cargo packages if available
if [ -d "$HOME/.cargo/bin" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# Set PATH so it includes user directory
if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

# Initialize Bash if available
if [ -n "$BASH" ]; then
    [ -r "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi
