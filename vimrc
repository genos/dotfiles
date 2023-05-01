"Security exploits?
set nomodeline
set modelines=0

if !has('nvim')
  set nocompatible " be iMproved
endif

"Plugin time: https://github.com/junegunn/vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.vim/plugged')
Plug 'dense-analysis/ale'
Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}
Plug 'ervandew/supertab'
Plug 'genos/quil-vim'
Plug 'godlygeek/tabular'
Plug 'joom/latex-unicoder.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'kaarmu/typst.vim'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

"Turn on stuff
filetype plugin indent on
syntax on

" always have a status line
set laststatus=2

"Python provider
let g:python3_host_prog = 'python3'

"Explicitly set clipboard (speeds things up by skipping system call)
let g:clipboard = { 'name': 'pbcopy',
                  \ 'copy': { '+': 'pbcopy', '*': 'pbcopy' },
                  \ 'paste': {'+': 'pbpaste', '*': 'pbpaste' },
                  \ 'cache_enabled': 0,
                  \ }

"Colors & highlighting
set t_Co=256
highlight LineNr ctermfg=DarkGrey
highlight Comment cterm=italic gui=italic
let g:airline_theme="molokai"

"Set encoding to utf-8
set enc=utf-8

"Each line follows the indentation of the line above
set autoindent

"Don't indent access specifiers or labels in C, C++
set cinoptions=L0,g0

"Show matching parenthesis, etc.
set showmatch

"Insert spaces instead of tabs
set expandtab

"Tab length is 2 spaces
set tabstop=2

"Use 2 spaces for << and >> commands
set shiftwidth=2

"Use shiftwidth instead of tabstop setting
set smarttab

"Line numbers
set number

"Line and column position
set ruler

"Highlight search match(es)
set hlsearch

"Incremental search as search is typed
set incsearch

"Use shift-tab to unindent
inoremap <S-Tab> <C-D>

"Don't include an extra space when joining lines
set nojoinspaces

"Perl/Python regexes instead of Vim's
nnoremap / /\v
vnoremap / /\v

"Ignore case in searching (unless specified)
set ignorecase
set smartcase

"Don't go off bottom of screen
set scrolloff=3

"Decently fast, since we've got a modern computer
set ttyfast

"Follow the leader
let mapleader = ","

"Quickly get out of searches
nnoremap <leader><space> :noh<cr>

"Jump screenlines, not lines of text
nnoremap j gj
nnoremap k gk

"Save on losing focus, in case we tab away
au FocusLost * :wa

"Tab-complete commands etc.
set wildmenu
set wildmode=longest,full

"Backspace all the things
set backspace=indent,eol,start

"Use the mouse!?
set mouse=a

"Move up the directory hierarchy until you find a tags file
set tags=tags;/

"Allow vim access to system clipboard
set clipboard=unnamed

"fzf speedily
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>r :Rg<CR>

"ALE gutter: always on, but don't highlight it
set signcolumn=yes
highlight clear SignColumn
"ALE linters & fixers
let g:ale_haskell_ormolu_executable = 'fourmolu'
let g:ale_linters = { 'haskell': ['cabal_ghc', 'hlint', 'hls'],
                    \ 'ocaml': ['ocamllsp'],
                    \ 'python': ['pyright', 'ruff'],
                    \ 'rust': ['analyzer'],
                    \ }
let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace'],
                   \ 'haskell': ['ormolu'],
                   \ 'ocaml': ['ocamlformat'],
                   \ 'python': ['black'],
                   \ 'rust': ['rustfmt'],
                   \ }
"ALE autocomplete
let g:ale_completion_enabled = 1
"go to definitions
nnoremap <leader>d :ALEGoToDefinition<CR>
nnoremap <leader>t :ALEGoToTypeDefinition<CR>
"go to next/previous ALE error
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
"run ALEFix
nmap <silent> <C-h> <Plug>(ale_fix)
