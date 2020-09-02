"Security exploits?
set nomodeline
set modelines=0

if !has('nvim')
  set nocompatible               " be iMproved
endif

"Plugin time: https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')
Plug 'dpwright/vim-tup'
Plug 'dhruvasagar/vim-table-mode'
Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}
Plug 'ervandew/supertab'
Plug 'godlygeek/tabular'
Plug 'jgdavey/tslime.vim', {'branch': 'main'}
Plug 'joom/latex-unicoder.vim'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'ntpeters/vim-better-whitespace'
Plug 'ollykel/v-vim'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vmchale/dhall-vim', { 'for': 'dhall' }
call plug#end()

"Turn on stuff
filetype plugin indent on
syntax on

set laststatus=2  " always

"Python providers
let g:python_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'

"Explicitly set clipboard (speeds things up by skipping system call)
let g:clipboard = { 'name': 'pbcopy',
                  \ 'copy': { '+': 'pbcopy', '*': 'pbcopy' },
                  \ 'paste': {'+': 'pbpaste', '*': 'pbpaste' },
                  \ 'cache_enabled': 0 }

"Colors & highlighting
set t_Co=256
highlight LineNr ctermfg=DarkGrey
highlight Comment cterm=italic gui=italic

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
set numberwidth=5

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

"Whip you into Vim shape!
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

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

"tslime.vim key mappings
vmap <C-c><C-c> <Plug>SendSelectionToTmux
nmap <C-c><C-c> <Plug>NormalModeSendToTmux
nmap <C-c>r <Plug>SetTmuxVars

"Move up the directory hierarchy until you find a tags file
set tags=tags;/

"Allow vim access to system clipboard
set clipboard=unnamed

"fzf speedily
nnoremap <leader>f :Files<CR>
nnoremap <leader>g :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>r :Rg<CR>
nnoremap <Leader>t :Tags<CR>

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
" ## added by OPAM user-setup for vim / ocp-indent ## 8c84786bf416bd863843dcc6cddc1491 ## you can edit, but keep this line
if count(s:opam_available_tools,"ocp-indent") == 0
  source "$HOME/.opam/default/share/ocp-indent/vim/indent/ocaml.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line
