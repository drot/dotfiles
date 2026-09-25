# shellcheck shell=sh

# Initialize Homebrew on Apple Silicon before configuring other tools.
if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Environment variables
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""
export VISUAL="${EDITOR}"
export PAGER="less"
export LESS="-Ri"
export GREP_COLORS="mt=01;37;41"
export NAME="Davor Rotim"
export EMAIL="d.rotim@sportradar.com"
export GROFF_NO_SGR=1

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

_profile_path_prepend "$HOME/.cargo/bin"
_profile_path_prepend "$HOME/go/bin"
_profile_path_prepend "$HOME/.opencode/bin"
_profile_path_prepend "$HOME/kafka-tools/bin"
_profile_path_prepend "$HOME/.local/bin"

export PATH
unset -f _profile_path_prepend

# Initialize Bash
if [ -n "${BASH_VERSION:-}" ] && [ -r "$HOME/.bashrc" ]; then
    # shellcheck disable=SC1091 # user-specific Bash configuration
    . "$HOME/.bashrc"
fi
