# Specify main configuration directory
ZDOTDIR="${ZDOTDIR:-$HOME/.zsh}"

# Global variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""
export VISUAL="${EDITOR}"
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export NAME="Davor Rotim"
export EMAIL="drot@firemail.cc"

# Qt5 default style
export QT_QPA_PLATFORMTHEME="qt5ct"

# Wine specific variables
export WINEPREFIX="$HOME/.config/wine"
export WINEARCH="win32"
export WINEDEBUG="-all"
# Disable Gecko install prompt; prevent .desktop creation
export WINEDLLOVERRIDES="winemenubuilder.exe,mshtml=d"

# TeXdoc specific variables
export PDFVIEWER_texdoc="llpp"
export DVIVIEWER_texdoc="${PDFVIEWER_texdoc}"
