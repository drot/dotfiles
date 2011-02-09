# Load colors
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi

# Auto completion
autoload -U compinit
compinit

# Prompt
autoload -U promptinit
promptinit

# Variables
unset SCREENDIR # fix screen
export PATH=${PATH}:${HOME}/bin # path for my executables
export EDITOR=emacsclient # default editor
export ALTERNATE_EDITOR=emacs # avoid trouble
export VISUAL=$EDITOR # compat
export PAGER=less # man page viewer

# No core dumps
limit coredumpsize 0

# Completion for cd and ssh
compctl -g '*(-/)' + -g '.*(-/)' -v cd pushd rmdir
compctl -k hosts -x 'p[2,-1]' -l '' -- rsh ssh

# Completion for man
compctl -f -x 'S[1][2][3][4][5][6][7][8][9]' -k '(1 2 3 4 5 6 7 8 9)' \
    - 'R[[1-9nlo]|[1-9](|[a-z]),^*]' -K 'match-man' \
    - 's[-M],c[-1,-M]' -g '*(-/)' \
    - 's[-P],c[-1,-P]' -c \
    - 's[-S],s[-1,-S]' -k '( )' \
    - 's[-]' -k '(a d f h k t M P)' \
    - 'p[1,-1]' -c + -K 'match-man' \
    -- man

# Colors for grep, man and ls
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32' # beautify grep
export GROFF_NO_SGR=1 # output ANSI color escape sequences in raw form
export LESS_TERMCAP_mb=$'\E[0;31m' # blinking
export LESS_TERMCAP_md=$'\E[1;34m' # bold, used for headings
export LESS_TERMCAP_us=$'\E[1;32m' # underline, used for paths,keywords
export LESS_TERMCAP_so=$'\E[41;1;37m' # standout, used for statusbar/search
export LESS_TERMCAP_ue=$'\E[0m' # end underline
export LESS_TERMCAP_se=$'\E[0m' # end standout-mode
export LESS_TERMCAP_me=$'\E[0m' # end all modes like so, us, mb, md and mr
eval `dircolors -b "${HOME}/.dircolors"` # dircolors

# Completion for following commands
compctl -b bindkey
compctl -v export
compctl -o setopt
compctl -v unset
compctl -o unsetopt
compctl -v vared
compctl -c which
compctl -c sudo

# History
HISTFILE=$HOME/.zshist
SAVEHIST=5000
HISTSIZE=5000

# Keybindings
bindkey -e
bindkey "^[[A" history-beginning-search-backward # Up
bindkey "^[[B" history-beginning-search-forward  # Down

# Aliases
alias ls="ls -h --group-directories-first --color=always"
alias ec="emacsclient -a emacs"
alias eckd="emacsclient -e '(kill-emacs)'"

# Select Prompt
zstyle ':completion:*' menu select=1

# Expansion options
zstyle ':completion:::::' completer _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) )'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'

# Completion caching
zstyle ':completion::complete:*' use-cache 1

# Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

# Include non-hidden directories in globbed file completions
# for certain commands
zstyle ':completion::complete:*' '\'

# Use menuselection for pid completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# Tag-order 'globbed-files directories' all-files
zstyle ':completion::complete:*:tar:directories' file-patterns '*~.*(-/)'

# Don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Separate matches into groups
zstyle ':completion:*:matches' group 'yes'

# With commands like rm, it's annoying if you keep getting offered the same
# file multiple times. This fixes it. Also good for cp, et cetera..
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

# Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'

# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

# Simulate spider's old abbrev-expand 3.0.5 patch
zstyle ':completion:*:history-words' stop verbose
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false

# Follow GNU LS_COLORS
zmodload -i zsh/complist
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:kill:*' list-colors '=%*=01;31'

# Subdivide man pages into sections
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*' group-name ''

# zsh Options
setopt                   \
    NO_all_export        \
    always_last_prompt   \
    always_to_end        \
    append_history       \
    auto_cd              \
    auto_list            \
    auto_menu            \
    auto_name_dirs       \
    auto_param_keys      \
    auto_param_slash     \
    auto_pushd           \
    auto_remove_slash    \
    NO_auto_resume       \
    bad_pattern          \
    bang_hist            \
    NO_beep              \
    brace_ccl            \
    correct_all          \
    NO_bsd_echo          \
    cdable_vars          \
    NO_chase_links       \
    clobber              \
    complete_aliases     \
    complete_in_word     \
    correct              \
    NO_correct_all       \
    csh_junkie_history   \
    NO_csh_junkie_loops  \
    NO_csh_junkie_quotes \
    NO_csh_null_glob     \
    equals               \
    extended_glob        \
    extended_history     \
    function_argzero     \
    glob                 \
    NO_glob_assign       \
    glob_complete        \
    NO_glob_dots         \
    glob_subst           \
    NO_hash_cmds         \
    NO_hash_dirs         \
    hash_list_all        \
    hist_allow_clobber   \
    hist_beep            \
    HIST_REDUCE_BLANKS   \
    hist_expire_dups_first \
    hist_find_no_dups    \
    hist_ignore_all_dups \
    hist_ignore_space    \
    NO_hist_no_store     \
    hist_verify          \
    NO_hup               \
    NO_ignore_braces     \
    NO_ignore_eof        \
    interactive_comments \
    inc_append_history   \
    NO_list_ambiguous    \
    NO_list_beep         \
    list_types           \
    long_list_jobs       \
    magic_equal_subst    \
    NO_mail_warning      \
    NO_mark_dirs         \
    menu_complete        \
    multios              \
    nomatch              \
    notify               \
    NO_null_glob         \
    numeric_glob_sort    \
    NO_overstrike        \
    path_dirs            \
    posix_builtins       \
    NO_print_exit_value  \
    NO_prompt_cr         \
    prompt_subst         \
    pushd_ignore_dups    \
    NO_pushd_minus       \
    pushd_silent         \
    pushd_to_home        \
    rc_expand_param      \
    NO_rc_quotes         \
    NO_rm_star_silent    \
    NO_sh_file_expansion \
    sh_option_letters    \
    short_loops          \
    NO_sh_word_split     \
    NO_single_line_zle   \
    NO_sun_keyboard_hack \
    transient_rprompt    \
    unset                \
    NO_verbose           \
    zle

# Window title
case $TERM in
    *xterm*|rxvt|rxvt-unicode|rxvt-256color|rxvt-unicode-256color|(dt|k|E)term)
    precmd () { print -Pn "\e]0;%n@%M %~ %#\a" }
    preexec () { print -Pn "\e]0;%n@%M %~ %# ($1)\a" }
    ;;
esac

# prompt
prompt gentoo
