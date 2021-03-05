#!/bin/bash

# Saner ls
alias ls="ls -h --group-directories-first --color=auto"

# Colorize grep results
alias grep="grep --color=auto"

# Paste to termbin
alias tb="socat - TCP4:termbin.com:9999"

# Find process info
alias pids="pgrep -a"

# Set SSH TERM workaround
alias ssh="TERM=xterm-256color ssh"

# SSH with default work port
alias ppsh="ssh -p 1044"
