set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'w0rp/ale'
Plugin 'rust-lang/rust.vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'othree/html5.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'Quramy/tsuquyomi'
Plugin 'leafgarland/typescript-vim'
Plugin 'purescript-contrib/purescript-vim'
Plugin 'FrigoEU/psc-ide-vim'
Plugin 'vimwiki/vimwiki'
Plugin 'fsharp/vim-fsharp'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-rhubarb'
Plugin 'tpope/vim-fireplace'

call vundle#end()            " required

filetype plugin indent on    " required
syntax on

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

set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

set list
set listchars=tab:⇥\ ,eol:¬

set t_vb=

nnoremap j gj
nnoremap k gk

nnoremap <leader>w <C-w>v<C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <leader>p :CtrlP<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>l :lopen<cr>
nnoremap <leader>L :lclose<cr>
nnoremap <leader>c :copen<cr>
nnoremap <leader>C :cclose<cr>

autocmd FileType typescript nnoremap <leader>r :TsuReferences<cr>
autocmd FileType typescript nnoremap <leader>d :TsuDefinition<cr>
autocmd FileType typescript nnoremap <leader>t : <C-u>echo tsuquyomi#hint()<CR>

autocmd FileType purescript nnoremap <leader>d :PSCIDEgoToDefinition<cr>
autocmd FileType purescript nnoremap <leader>t :PSCIDEtype<cr>
autocmd FileType purescript nnoremap <leader>a :PSCIDEapplySuggestion<cr>

autocmd FileType xml nnoremap <leader>f :%!xmllint --format %<cr>

let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

set background=dark
colorscheme solarized

" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

let g:tsuquyomi_disable_quickfix = 1

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" let g:syntastic_javascript_checkers = ['eslint']
" let g:syntastic_javascript_eslint_exec = './node_modules/.bin/eslint'
" let g:syntastic_typescript_checkers = ['tsuquyomi', 'tslint']
" let g:syntastic_typescript_tslint_exec = './node_modules/.bin/tslint --project'
" let g:syntastic_fsharp_checkers = ['']

let g:ale_linters = {}
let g:ale_linters['typescript'] = ['prettier', 'tslint', 'tsserver']

let g:ale_fixers = {}
let g:ale_fixers['typescript'] = ['prettier']

let g:ale_javascript_prettier_use_local_config = 1
let g:ale_typescript_prettier_use_local_config = 1

let g:EditorConfig_exclude_patterns = ['fugitive://.*']
