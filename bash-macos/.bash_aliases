#!/opt/homebrew/bin/bash

# Native macOS ls: human-readable sizes and colored output
alias ls="ls -Gh"

# Colorize grep results
alias grep="grep --color=auto"

# Paste to termbin
alias tb="socat - TCP4:termbin.com:9999"

# Paste to tcp.st
alias tcpst="socat - OPENSSL:tcp.st:8777"

# Find process info using the full argument list
alias pids="pgrep -lf"

# SSH with different port
alias ppsh="ssh -p 1044"

# Save tmux pane contents
alias tmux-save-pane="tmux capture-pane -pS -"
