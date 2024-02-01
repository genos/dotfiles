"Security exploits?
set nomodeline
set modelines=0

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
Plug 'godlygeek/tabular'
Plug 'joom/latex-unicoder.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'mlochbaum/BQN', {'rtp': 'editors/vim'}
Plug 'sheerun/vim-polyglot'
Plug 'tomasr/molokai'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vlime/vlime', {'rtp': 'vim/'}
call plug#end()

"Python provider
let g:python3_host_prog = 'python3'

"Explicitly set clipboard (speeds things up by skipping system call)
let g:clipboard = { 'name': 'pbcopy',
                  \ 'copy': { '+': 'pbcopy', '*': 'pbcopy' },
                  \ 'paste': {'+': 'pbpaste', '*': 'pbpaste' },
                  \ 'cache_enabled': 0,
                  \ }

"Colors & highlighting
set termguicolors
colorscheme molokai
let g:airline_theme="molokai"
let g:molokai_original = 1
highlight Comment cterm=italic gui=italic

set enc=utf-8                  " Set encoding to utf-8
set showmatch                  " Show matching parenthesis, etc.
set expandtab                  " Insert spaces instead of tabs
set tabstop=2                  " Tab length is 2 spaces
set shiftwidth=2               " Use 2 spaces for << and >> commands
set smarttab                   " Use shiftwidth instead of tabstop setting
set number                     " Line numbers
set ignorecase                 " Ignore case in searching (unless specified)
set smartcase                  " Ignore case in searching (unless specified)
set ttyfast                    " Decently fast, since we've got a modern computer
set wildmode=longest,full      " Tab-complete commands etc.
set mouse=a                    " Use the mouse!?
set clipboard=unnamed          " Allow vim access to system clipboard
let mapleader = ","            " Following the leader

"Perl/Python regexes instead of Vim's
nnoremap / /\v
vnoremap / /\v
"Quickly get out of searches
nnoremap <leader><space> :noh<cr>
"Jump screenlines, not lines of text
nnoremap j gj
nnoremap k gk
"Save on losing focus, in case we tab away
au FocusLost * :wa

"fzf speedily
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>r :Rg<CR>

"ALE gutter: always on, but don't highlight it
set signcolumn=yes
highlight clear SignColumn
"ALE linters & fixers
let g:ale_linters = { 'haskell': ['cabal_ghc', 'hlint', 'hls'],
                    \ 'clojure': ['clj-kondo'],
                    \ 'ocaml': ['ocamllsp'],
                    \ 'python': ['pyright', 'ruff'],
                    \ 'rust': ['analyzer'],
                    \ }
let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace'],
                   \ 'haskell': ['fourmolu'],
                   \ 'ocaml': ['ocamlformat'],
                   \ 'python': ['black'],
                   \ 'rust': ['rustfmt'],
                   \ }
"ALE use virtualenvs
let g:ale_python_auto_poetry = 1
let g:ale_python_auto_virtualenv = 1
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
