" Disable vi compatibility
set nocompatible

" Plugin installation
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'jnurmine/Zenburn'

call plug#end()

" Hide buffers
set hidden

" Maintain undo history
set undofile
set undodir=~/.vim/undo

" Lazy file name tab completion
set wildmenu
set wildignorecase

" Use correct indentation
filetype indent plugin on

" Enable syntax highlighting
syntax on

" Enable auto indentation
set autoindent

" Use indentation with 4 spaces
set shiftwidth=4
set softtabstop=4

" Insert spaces instead of tabs
set expandtab

" More powerful backspace
set backspace=indent,eol,start

" Paste without auto indentation
set paste

" Use case insensitive search
set ignorecase
set smartcase

" Immediate search
set incsearch

" Disable the startup message
set shortmess+=I

" Show current cursor position
set ruler

" Show line numbers
set number

" Show partial commands
set showcmd

" Highlight cursor line
set cursorline

" Highlight searches
set hlsearch

" Show matching brackets
set showmatch

" Display the status line
set laststatus=2

" Set colorscheme
if &t_Co > 255 || has('gui_running')
    set background=dark
    colorscheme zenburn
endif

" Toggle undo visualization
nnoremap <F5> :UndotreeToggle<CR>
