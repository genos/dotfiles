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
Plug 'factor/factor.vim'
Plug 'godlygeek/tabular'
Plug 'https://codeberg.org/growler/k', {'rtp': 'vim-k' }
Plug 'joom/latex-unicoder.vim'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'sheerun/vim-polyglot'
Plug 'tidalcycles/vim-tidal'
Plug 'tomasr/molokai'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
call plug#end()

"Turn off some optional things
let g:loaded_node_provider = 0
let g:loaded_ruby_provider = 0
let g:loaded_perl_provider = 0
let g:loaded_python3_provider = 0

"Explicitly set clipboard (speeds things up by skipping system call)
let g:clipboard = { 'name': 'pbcopy',
                  \ 'copy': { '+': 'pbcopy', '*': 'pbcopy' },
                  \ 'paste': {'+': 'pbpaste', '*': 'pbpaste' },
                  \ 'cache_enabled': 0, }

"Colors & highlighting
set termguicolors
colorscheme molokai
let g:molokai_original = 1
highlight Comment cterm=italic gui=italic

"Various settings
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
autocmd FocusLost * :wa        " Save on losing focus, in case we tab away

"Perl/Python regexes instead of Vim's
nnoremap / /\v
vnoremap / /\v
"Quickly get out of searches
nnoremap <leader><space> :noh<cr>
"Jump screenlines, not lines of text
nnoremap j gj
nnoremap k gk

"fzf speedily
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>r :Rg<CR>

"ALE gutter: always on, but don't highlight it
set signcolumn=yes
highlight clear SignColumn
"ALE linters & fixers
let g:ale_linters = {
                    \ 'clojure': ['clj-kondo', 'joker'],
                    \ 'elm': ['elm_ls'],
                    \ 'haskell': ['hlint', 'hls'],
                    \ 'ocaml': ['ocamllsp'],
                    \ 'python': ['pyright', 'ruff'],
                    \ 'rust': ['analyzer'],
                    \ 'typescript': ['deno'],
                    \}
let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace'],
                   \ 'elm': ['elm-format'],
                   \ 'haskell': ['fourmolu'],
                   \ 'nix': ['alejandra'],
                   \ 'ocaml': ['ocamlformat'],
                   \ 'python': ['ruff', 'ruff_format'],
                   \ 'rust': ['rustfmt'],
                   \ 'typescript': ['deno'],
                   \}
"ALE rust format edition
let g:ale_rust_rustfmt_options = '--edition 2024'
"ALE use uv virtualenvs
let g:ale_python_auto_uv = 1
"ALE autocomplete
let g:ale_completion_enabled = 1
set completeopt+=noinsert
"go to definitions
nnoremap <leader>d :ALEGoToDefinition<CR>
nnoremap <leader>t :ALEGoToTypeDefinition<CR>
"go to next/previous ALE error
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
"run ALEFix
nmap <silent> <C-h> <Plug>(ale_fix)

" Vim-slime
let g:slime_target = "tmux"
