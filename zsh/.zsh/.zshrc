# Shell behavior options
setopt autocd
setopt extendedglob
setopt nomatch
setopt completealiases
setopt correct
setopt no_correctall

# History size
export HISTFILE="$ZDOTDIR/.zhistory"
export HISTSIZE=10000
export SAVEHIST=$((HISTSIZE/2))
# History options
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY

# Edit history
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Completions
autoload -Uz compinit colors
compinit
colors

# Load completion extensions
zmodload zsh/complist

# Completion options
zstyle ':completion:*' completer _complete _correct _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' completer _expand_alias _complete _approximate
zstyle ':completion:*' menu select
zstyle ':completion:*' file-sort name
zstyle ':completion:*' ignore-parents pwd
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Colored listings
if [[ -f ~/.dircolors ]]; then
    eval $(dircolors -b ~/.dircolors)
fi

# Prompt
LPROMPT () {
    PS1="┌─[%{$fg[cyan]%}%m%{$fg_bold[blue]%} %~%{$fg_no_bold[yellow]%}%(0?.. %?)%{$reset_color%}]
└─╼ "
}
LPROMPT
PROMPT_EOL_MARK=" •"
