" general
set nocp
set autoread
set nobackup
set noswapfile
set nowrap
syntax on
 
" search
set incsearch
set showmatch
set hlsearch
set ignorecase
set smartcase

" indenting
set autoindent
filetype plugin indent on

" wild menu
set wildmenu
set wildmode=list:longest,full

" look 
set t_Co=256
colorscheme neverland
set cursorline
set showcmd
set showmode
set number

" statusline
set statusline=%<%f\ %y%h%m%r\ PWD:%{getcwd()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2
