" Disable vi compatibility
set nocompatible

" Plugin installation
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'xero/sourcerer.vim'

call plug#end()

" Use correct indentation
filetype indent plugin on

" Enable syntax highlighting
syntax on

" Hide buffers
set hidden

" Maintain undo history
set undofile
set undodir=~/.vim/undo

" Lazy file name tab completion
set wildmode=longest,list,full
set wildmenu
set wildignorecase

" Enable auto indentation
set autoindent

" Use indentation with 4 spaces
set shiftwidth=4
set softtabstop=4
set expandtab

" More powerful backspace
set backspace=indent,eol,start

" Paste without auto indentation
set paste

" Use smart case insensitive search
set ignorecase
set smartcase

" Disable the startup message
set shortmess+=I

" Show partial commands
set showcmd

" Show current cursor position
set ruler

" Enable crosshair cursor
set cursorline
set cursorcolumn

" Show line numbers
set number

" Highlight searches
set hlsearch

" Show matching brackets
set showmatch

" Display the status line
set laststatus=2

" Set colorscheme
if &t_Co > 255 || has('gui_running')
    set background=dark
    colorscheme sourcerer
endif

" Toggle the NERD tree
nnoremap <F4> :NERDTreeToggle<cr>

" Toggle the undo tree
nnoremap <F5> :UndotreeToggle<cr>
