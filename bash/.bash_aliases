#!/bin/bash

# Safer file handling operations
alias cp="cp --interactive"
alias mv="mv --interactive"
alias rm="rm -I"

# Make subdirectories automatically and notify about creation
alias mkdir="mkdir --parents --verbose"

# Saner ls
alias ls="ls -h --group-directories-first --color=auto"

# Colorize grep results
alias grep="grep --color=auto"

# Paste to termbin
alias tb="nc termbin.com 9999"

# Find process info
alias pids="pgrep -a"
