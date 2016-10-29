#!/bin/bash

# Paste to 0x0.st
0x0() {
    curl -F "file=@${1:--}" https://0x0.st
}

# Paste to sprunge.us
sprunge() {
    curl -F "sprunge=<-" http://sprunge.us
}

# Find process
pids() {
    ps aux | grep -i $1
}
