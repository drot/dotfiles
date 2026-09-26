# Specify main configuration directory
ZDOTDIR="${ZDOTDIR:-$HOME/.config/zsh}"

# Environment variables
# EDITOR opens in the terminal, VISUAL in a new GUI frame (most tools prefer VISUAL);
# an empty ALTERNATE_EDITOR starts the Emacs daemon if it isn't running
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=""
export VISUAL="emacsclient -c"
export PAGER="less"
# -R colors, -i smart-case search, -F quit if output fits one screen,
# -X don't clear the screen on exit
export LESS="-RiFX"
export GREP_COLORS="mt=01;37;41"
export NAME="Davor Rotim"
export EMAIL="d.rotim@sportradar.com"
