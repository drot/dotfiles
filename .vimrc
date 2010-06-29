" -[ General behaviour ]-
" general
set nocp
set autoread
set nobackup
set noswapfile
set nowrap
syntax on
filetype plugin indent on
 
" search
set incsearch
set showmatch
set hlsearch
set ignorecase
set smartcase

" indenting
set autoindent
set smartindent
set smarttab

" wild menu
set wildmenu
set wildmode=list:longest,full

" -[ Look ]-
" general
set showcmd
set showmode
set number
" statusline
set statusline=%<%f\ %y%h%m%r\ PWD:%{getcwd()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2
