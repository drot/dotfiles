# Environment variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""
export VISUAL="${EDITOR}"
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export NAME="Davor Rotim"
export EMAIL="rotim.davor@nsoft.com"
export GROFF_NO_SGR=yes

# Qt5 default style
export QT_QPA_PLATFORMTHEME="qt5ct"

# Check for Cargo binaries if available
[[ -d $HOME/.cargo/bin ]] && export PATH="$HOME/.cargo/bin:$PATH"

# Set PATH so it includes user directory
[[ -d $HOME/.local/bin ]] && export PATH="$HOME/.local/bin:$PATH"

# Initialize Bash
[[ -r ~/.bashrc ]] && source ~/.bashrc
