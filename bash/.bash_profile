# Environment variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""
export VISUAL="${EDITOR}"
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export NAME="Davor Rotim"
export EMAIL="rotim.davor@nsoft.com"
export SBCL_HOME="${HOME}/.local/lib/sbcl"
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gcr/ssh"

# Qt5 default style
export QT_QPA_PLATFORMTHEME="qt5ct"

# Wine specific variables
export WINEPREFIX="${HOME}/.config/wine"
export WINEARCH="win32"
export WINEDEBUG="-all"
# Disable Gecko install prompt; prevent .desktop creation
export WINEDLLOVERRIDES="winemenubuilder.exe,mshtml=d"

# Check for Cargo binaries if available
[[ -d $HOME/.cargo/bin ]] && export PATH="${HOME}/.cargo/bin:$PATH"

# Check for Go binaries if available
[[ -d $HOME/go/bin ]] && export PATH="${HOME}/go/bin:$PATH"

# Set PATH so it includes user directory
[[ -d $HOME/.local/bin ]] && export PATH="${HOME}/.local/bin:$PATH"

# Initialize Bash
[[ -r ~/.bashrc ]] && source ~/.bashrc
