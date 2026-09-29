# Config directory
ZDOTDIR="${ZDOTDIR:-$HOME/.config/zsh}"

# Editor: -t in terminal, -c new GUI frame (most tools use VISUAL)
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="" # start Emacs daemon if not running
export VISUAL="emacsclient -c"

# Pager: colors, smart-case search, quit if output fits one screen
export PAGER="less"
export LESS="-RiF"

# Grep matches as bold red blocks (GREP_COLOR: macOS grep, GREP_COLORS: GNU grep)
export GREP_COLOR="01;07;31"
export GREP_COLORS="mt=$GREP_COLOR"

# Identity (Emacs, git)
export NAME="Davor Rotim"
export EMAIL="d.rotim@sportradar.com"
