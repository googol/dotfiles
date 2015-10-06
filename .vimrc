set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'othree/html5.vim'
Plugin 'kien/ctrlp.vim'

call vundle#end()            " required

filetype plugin indent on    " required

set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set modelines=0

set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set visualbell
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2
set undofile

let mapleader=","

nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

set list
set listchars=tab:⇥\ ,eol:¬

nnoremap j gj
nnoremap k gk

nnoremap <leader>w <C-w>v<C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>p :CtrlP<cr>
