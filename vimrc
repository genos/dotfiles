"Set encoding to utf-8
set enc=utf-8

"Each line follows the indentation of the line above
set autoindent

"Guess indent level of any new line
set smartindent

"Show matching parenthesis, etc.
set showmatch

"Insert spaces instead of tabs
set expandtab

"Tab length is 4 spaces
set tabstop=4

"Use 4 spaces for << and >> commands
set shiftwidth=4

"Use shiftwidth instead of tabstop setting
set smarttab

"Line numbers
set number

"For compatibility with screen
set numberwidth=8

"Line and column position
set ruler

"Syntax highlighting
syntax on

"Filetype detection
filetype on

"Load plugins for specific filetypes
filetype plugin on

"Load indentation for specific filetypes
filetype indent on

"Highlight search match(es)
set hlsearch

"Incremental search as search is typed
set incsearch

"Use shift-tab to unindent
inoremap <S-Tab> <C-D>

"Highlights once a line is past 79 characters
match ErrorMsg '\%>79v.\+'

" find Doxygen's todo tag
2match Search '\(\\\|@\)todo'

"""""""
" Sage settings (from Franco Saliola via wiki.sagemath.org/Tips)
autocmd BufRead,BufNewFile *.sage,*.spyx set filetype=python

"""""""
" Clojure settings (from Clojure Google group)
autocmd BufRead,BufNewFile *.clj set filetype=clojure

"""""""
" Scheme, Lisp, and Clojure: paren-matching
autocmd BufRead,BufNewFile *.scm,*.lsp,*.lisp,*.cl,*.clj set sm

"""""""
" Use gpg to encipher and decipher what you're working on.
" NOTE: Not as secure as it could be; swap's not encrypted, still writes to
" .viminfo, etc.
" from http://vim.wikia.com/wiki/AES256_encryption_in_Vim
function! Scramble()
    %!gpg -q --cipher-algo aes256 --symmetric --armor 2>/dev/null
endfunction

function! Unscramble()
    %!gpg -q --cipher-algo aes256 --decrypt --armor 2>/dev/null
endfunction

"""""""
" Google's Go language
set rtp+=/usr/local/go/misc/vim
au BufRead,BufNewFile *.go set filetype=go
au BufRead,BufNewFile *.go set noexpandtab
