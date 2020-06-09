set nocompatible
filetype on
filetype plugin on
filetype indent on
set hidden
set showcmd
set showmode
set wildmenu
set noerrorbells
set noswapfile
set ls=2
set backspace=indent,eol,start
set bg=dark
set relativenumber
set tabstop=4
set shiftwidth=4
set expandtab
set incsearch
set hlsearch
set ruler
syntax enable
autocmd BufEnter *.elm :setlocal filetype=ocaml
