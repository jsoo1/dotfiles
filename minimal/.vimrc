set nocompatible
filetype on
filetype plugin on
filetype indent on
set hidden
set showcmd
set showmode
set wildmenu
set noerrorbells
set ls=2
set backspace=indent,eol,start
set bg=dark
set number
set noruler
syntax enable
set expandtab
set tabstop=2
set softtabstop=4
set shiftwidth=4
set hlsearch
set incsearch
set backupdir=~/.local/share/vim/backup
set splitright
system('mkdir -p ~/local/share/vim/backup')

if executable("rg")
  set grepprg=rg\ --vimgrep\ --smart-case\ --hidden
  set grepformat=%f:%l:%c:%m
endif
