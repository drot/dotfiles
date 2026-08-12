# shellcheck shell=sh

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
export GROFF_NO_SGR=1

# Qt5 default style
export QT_QPA_PLATFORMTHEME="qt5ct"

# Wine specific variables
export WINEPREFIX="${HOME}/.config/wine"
export WINEARCH="win32"
export WINEDEBUG="-all"
# Disable Gecko install prompt; prevent .desktop creation
export WINEDLLOVERRIDES="winemenubuilder.exe,mshtml=d"

_profile_path_prepend () {
    [ -d "$1" ] || return

    # Remove every existing exact occurrence
    while :; do
        # shellcheck disable=SC2123 # temporarily empty before prepending
        case $PATH in
            "$1") PATH= ;;
            "$1":*) PATH=${PATH#*:} ;;
            *:"$1":*) PATH=${PATH%%:"$1":*}:${PATH#*:"$1":} ;;
            *:"$1") PATH=${PATH%:*} ;;
            *) break ;;
        esac
    done

    PATH="$1${PATH:+:$PATH}"
}

# Check for Cargo binaries if available
_profile_path_prepend "$HOME/.cargo/bin"

# Check for Go binaries if available
_profile_path_prepend "$HOME/go/bin"

# opencode
_profile_path_prepend "$HOME/.opencode/bin"

# Kafka tools
[ -d "$HOME/kafka-tools/bin" ] && {
    export CLASSPATH="${HOME}/kafka-tools/libs/aws-msk-iam-auth-2.3.5-all.jar${CLASSPATH:+:$CLASSPATH}"
    _profile_path_prepend "$HOME/kafka-tools/bin"
}

# Set PATH so it includes user directory
_profile_path_prepend "$HOME/.local/bin"

export PATH
unset -f _profile_path_prepend

# Initialize Bash
if [ -n "${BASH_VERSION:-}" ] && [ -r "$HOME/.bashrc" ]; then
    # shellcheck disable=SC1091 # user-specific Bash configuration
    . "$HOME/.bashrc"
fi
