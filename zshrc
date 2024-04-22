# Initialize colors.
autoload -U colors && colors
autoload -Uz vcs_info
precmd() { vcs_info }
PROMPT='%F{cyan}%~%f %F{green}∃%f '
setopt prompt_subst  # The prompt string is first subjected to parameter expansion, command substitution and arithmetic expansion.
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*:*' stagedstr '●'
zstyle ':vcs_info:*:*' unstagedstr '●'
zstyle ':vcs_info:*:*' formats '%F{green}%c%f%F{red}%u%f %F{cyan}%b%f'
zstyle ':vcs_info:*:*' actionformats '%F{green}%c%f%F{red}%u%f %F{cyan}%b%f%F{magenta}(%a)%f'

alias -g ....='../../..'
alias -g ...='../..'
alias apl='/Applications/Dyalog-19.0.app/Contents/Resources/Dyalog/mapl'
alias cp='nocorrect cp'
alias e='$EDITOR'
alias g='git'
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gca='git commit --all'
alias gco='git checkout'
alias gd='git diff'
alias gl='git pull'
alias glol='git log --graph --decorate --oneline'
alias gm='git merge'
alias gp='git push'
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias gst='git status'
alias gwch='git whatchanged -p --abbrev-commit --pretty=medium'
alias ip='ipython'
alias ipn='jupyter notebook'
alias jc='/Applications/j9.6/bin/jconsole'
alias kc="rlwrap $HOME/bin/repl.k"
alias l='eza --git --header --long'
alias la='eza --all --git --header --long'
alias ln='nocorrect ln'
alias lrt='eza --long --sort=modified'
alias ls='ls -G'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias pjc='poetry run jupyter console'
alias pjn='poetry run jupyter notebook'
alias pri='poetry run ipython'
alias prp='poetry run python'
alias py='python3'
alias rjc='rye run jupyter console'
alias rjn='rye run jupyter notebook'
alias rri='rye run ipython'
alias rrp='rye run python'
alias ssh='TERM=xterm-256color ssh'
alias touch='nocorrect touch'
alias vlime="sbcl --load $HOME/.vim/plugged/vlime/lisp/start-vlime.lisp"

bindkey "^X^I" expand-or-complete-prefix # Attempt shell expansion on the current word up to cursor.
bindkey -v  # Selects keymap 'viins' for any operations by the current command, and also links 'viins' (vi emulation - insert mode) to 'main' so that it is selected by default the next time the editor starts.

export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
export EDITOR='nvim'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_COMMAND="fd --type file"
export GOPATH="$HOME/go"
export HISTSIZE=10000
export KEYTIMEOUT=1
export LANG=en_US.UTF-8
export MODULAR_HOME="$HOME/.modular"
export LC_ALL=en_US.UTF-8
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/"
export PYENV_ROOT="$HOME/.pyenv"
export SAVEHIST=10000
export SHELL="/usr/local/bin/zsh"

fpath=(
  $HOME/site-functions
  $fpath
)
typeset -U fpath
path=(
  $HOME/bin
  $HOME/.local/bin
  $HOME/.cargo/bin
  $MODULAR_HOME/pkg/packages.modular.com_mojo/bin
  $PYENV_ROOT/bin
  $GOPATH/bin
  /opt/homebrew/opt/llvm/bin
  /Library/TeX/Distributions/Programs/texbin
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /usr/sbin
  /bin
  /sbin
  $path
)
typeset -U path

function check_writing() {
  # Run the plaintext-ish file through various writing checks
  if [[ -n $1 ]]; then
    weasel $1 && passive $1 && dups $1 && alex $1
  else
    print "usage: $0 <file to check>"
  fi
}

function gi() {
  # Generate .gitignore file
  curl -sL https://www.toptal.com/developers/gitignore/api/$@
}

function hs() {
  # Search history with ripgrep
  history | rg "$*"
}

function man() {
  # Pretty manpages
  env \
    LESS_TERMCAP_mb="$(printf "\e[1;31m")" \
    LESS_TERMCAP_md="$(printf "\e[1;31m")" \
    LESS_TERMCAP_me="$(printf "\e[0m")" \
    LESS_TERMCAP_se="$(printf "\e[0m")" \
    LESS_TERMCAP_so="$(printf "\e[1;44;33m")" \
    LESS_TERMCAP_ue="$(printf "\e[0m")" \
    LESS_TERMCAP_us="$(printf "\e[1;32m")" \
    PAGER="${commands[less]:-$PAGER}" \
    _NROFF_U=1 \
    PATH="$HOME/bin:$PATH" \
    man "$@"
}

## Command history configuration
if [ -z "$HISTFILE" ]; then
  HISTFILE=$HOME/.zsh_history
fi

setopt auto_cd                # If a command is issued that can’t be executed as a normal command, and the command is the name of a directory, perform the cd command to that directory
setopt auto_pushd             # Make cd push the old directory onto the directory stack.
setopt pushd_ignore_dups      # Don’t push multiple copies of the same directory onto the directory stack.
setopt pushdminus             # Exchanges the meanings of ‘+’ and ‘-’ when used with a number to specify a directory in the stack.
unsetopt menu_complete        # Do not autoselect the first completion entry
unsetopt flowcontrol          # Output flow control via start/stop characters (usually assigned to ^S/^Q) is disabled in the shell’s editor.
setopt auto_menu              # Show completion menu on successive tab press
setopt complete_in_word       # Don't set the cursor to the end of the word if completion is started.
setopt always_to_end          # If a completion is performed with the cursor within a word, and a full completion is inserted, the cursor is moved to the end of the word.
setopt append_history         # Zsh sessions will append their history list to the history file, rather than replace it.
setopt extended_history       # Save each command’s beginning timestamp (in seconds since the epoch) and the duration (in seconds) to the history file.
setopt hist_expire_dups_first # If the internal history needs to be trimmed to add the current command line, setting this option will cause the oldest history event that has a duplicate to be lost before losing a unique event from the list.
setopt hist_ignore_dups       # Ignore duplication command history list.
setopt hist_ignore_space      # Remove command lines from the history list when the first character on the line is a space, or when one of the expanded aliases contains a leading space.
setopt hist_verify            # Whenever the user enters a line with history expansion, don’t execute the line directly; instead, perform history expansion and reload the line into the editing buffer.
setopt inc_append_history     # Incrementally append history.
unsetopt share_history        # Don't immediately pick up new commands from other concurrent shells.
setopt no_list_beep           # Disable bell when tab completing.

zstyle ':completion:*' list-colors ''  # Color the completion options.
zstyle ':completion:*' menu select     # Offer up a menu.
# Case-insensitive partial word and substring completion.
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' completer _expand _complete _approximate _ignored
# Kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
autoload -Uz compinit
if [[  (-e $HOME/.zcompdump) && ($(date +'%j') != $(stat -f '%Sm' -t '%j' $HOME/.zcompdump)) ]]; then
  compinit
else
  compinit -C
fi

# homebrew
[[ -f /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"

# zoxide for moving around
if command -v zoxide 1>/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

# brew install zsh-completions
if command -v brew 1>/dev/null 2>&1; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
fi

# fzf for fuzzy file finding
[[ -f $HOME/.fzf.zsh ]] && source $HOME/.fzf.zsh

# ghcup configuration
[[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ]] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

# rye configuration
[[ -f $HOME/.rye/env ]] && source $HOME/.rye/env

# pyenv configuration
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# rbenv configuration
if command -v rbenv 1>/dev/null 2>&1; then
  eval "$(rbenv init - --no-rehash)"
fi

# rustup configuration
[[ -f $HOME/.cargo/env ]] && source $HOME/.cargo/env

# opam configuration
[[ ! -r $HOME/.opam/opam-init/init.zsh ]] || source $HOME/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# zsh syntax highlighting
[[ -f $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# perl configuration
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"

# direnv hook
eval "$(direnv hook zsh)"

# goog cloud
[[ -f "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc" ]] && source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"
[[ -f "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc" ]] && source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"
