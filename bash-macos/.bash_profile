# shellcheck shell=bash

# Keep login-shell setup in the POSIX-compatible profile.
# shellcheck disable=SC1090 # user-specific profile
[[ -r ~/.profile ]] && source ~/.profile
