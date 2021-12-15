#!/bin/bash

# Saner ls
alias ls="ls -h --group-directories-first --color=auto"

# Colorize grep results
alias grep="grep --color=auto"

# Paste to termbin
alias tb="socat - TCP4:termbin.com:9999"

# Paste to tcp.st
alias tcpst="socat - OPENSSL:tcp.st:8777"

# Find process info
alias pids="pgrep -a"

# SSH with different port
alias ppsh="ssh -p 1044"
