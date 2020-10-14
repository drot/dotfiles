# Prevent duplicate entries in relevant paths
typeset -U path
typeset -U manpath
typeset -U infopath

# Set PATH so it includes user directory
path+=($HOME/.local/bin)

# TeX Live installation path
if [[ -d $HOME/.local/texlive ]]; then
    path+=($HOME/.local/texlive/2020/bin/x86_64-linux)
    manpath+=($HOME/.local/texlive/2020/texmf-dist/doc/man)
    infopath+=($HOME/.local/texlive/2020/texmf-dist/doc/info)
fi
