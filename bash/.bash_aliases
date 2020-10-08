#!/bin/bash

# Saner ls
alias ls="ls -h --group-directories-first --color=auto"

# Colorize grep results
alias grep="grep --color=auto"

# Paste to termbin
alias tb="ncat termbin.com 9999"

# Find process info
alias pids="pgrep -a"

# Set ssh TERM workaround for tmux
[[ -n $TMUX ]] && alias ssh="TERM=xterm-256color ssh"
