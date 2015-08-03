#
# ~/.bash_profile
#

export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export VISUAL=${EDITOR}
export PAGER=less
export BROWSER=conkeror
export PATH=$HOME/.local/bin:$PATH

[[ -f ~/.bashrc ]] && . ~/.bashrc
