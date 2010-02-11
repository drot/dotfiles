" -[ General behaviour ]-
" general
set nocompatible
set nowrap
syntax on
filetype plugin indent on

" search
set nohls
set incsearch
set showmatch

" indenting
set autoindent
set smartindent

" command mode
set wildmenu
set wildmode=list:longest,full

" -[ Look ]-
" general
colorscheme dante
set showcmd
set showmode
set number
 
" statusline
set statusline=%<%f\ %y%h%m%r\ PWD:%{getcwd()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2
