# Environment variables
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

# TeX Live installation
if [[ -d $HOME/.local/texlive ]]; then
    export PATH="$HOME/.local/texlive/2020/bin/x86_64-linux:$PATH"
    export MANPATH="$HOME/.local/texlive/2020/texmf-dist/doc/man:$MANPATH"
    export INFOPATH="$HOME/.local/texlive/2020/texmf-dist/doc/info:$INFOPATH"
fi

# Check for Cargo binaries if available
[[ -d $HOME/.cargo/bin ]] && export PATH="$HOME/.cargo/bin:$PATH"

# Set PATH so it includes user directory
[[ -d $HOME/.local/bin ]] && export PATH="$HOME/.local/bin:$PATH"

# Initialize Bash
[[ -r ~/.bashrc ]] && source ~/.bashrc
