source $HOME/.zshprompt

alias -g ....='../../..'
alias -g ...='../..'
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
alias l='exa --git --header --long'
alias la='exa --all --git --header --long'
alias ln='nocorrect ln'
alias lrt='exa --long --sort=modified'
alias ls='ls -G'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias pjc='poetry run jupyter console'
alias pjn='poetry run jupyter notebook'
alias pri='poetry run ipython'
alias prp='poetry run python'
alias py='python3'
alias ssh='TERM=xterm-256color ssh'
alias touch='nocorrect touch'

bindkey "^X^I" expand-or-complete-prefix
bindkey -v

export AWS_PROFILE=qml-qec
export BUN_INSTALL="$HOME/.bun"
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export EDITOR='nvim'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_COMMAND="fd --type file"
export GOPATH="$HOME/go"
export HISTSIZE=10000
export KEYTIMEOUT=1
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LESS='ciMQRX'
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
  $PYENV_ROOT/bin
  $GOPATH/bin
  $BUN_INSTALL/bin
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /usr/sbin
  /bin
  /sbin
  /Library/TeX/Distributions/Programs/texbin
  /usr/local/opt/python@3/bin
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
  # generate .gitignore file
  curl -sL https://www.toptal.com/developers/gitignore/api/$@
}

function hs() {
  # Search history with ripgrep
  history | rg "$*"
}

function man() {
  # pretty manpage
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

function nix_haskell() {
  # Open up a nix environment with optional packages
  # - http://alpmestan.com/posts/2017-09-06-quick-haskell-hacking-with-nix.html
  # - https://nixos.org/manual/nixpkgs/stable/#haskell
  pkgs=$@
  if [[ $# -gt 0 ]];
  then
    print "Starting haskell shell, pkgs = $pkgs"
    nix-shell --pure -p "haskellPackages.ghcWithPackages (ps: with ps; [$pkgs])"
  else
    print "Starting haskell shell"
    nix-shell --pure -p "haskellPackages.ghc"
	fi
}

function zsh_stats() {
  # Get a breakdown of most common zsh commands
  fc -l 1 | \
    awk '{CMD[$2]++;count++;}END{for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | \
    rg -v "./" | \
    column -c3 -s " " -t | \
    sort -nr | \
    nl | \
    head -n20
}

## Command history configuration
if [ -z "$HISTFILE" ]; then
  HISTFILE=$HOME/.zsh_history
fi

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus
unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on successive tab press
setopt complete_in_word
setopt always_to_end
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
autoload -Uz zmv
autoload -Uz compinit
if [[  (-e $HOME/.zcompdump) && ($(date +'%j') != $(stat -f '%Sm' -t '%j' $HOME/.zcompdump)) ]]; then
  compinit
else
  compinit -C
fi

if command -v zoxide 1>/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

[[ -f $HOME/.fzf.zsh ]] && source $HOME/.fzf.zsh

[[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ]] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path --no-rehash)"
fi

if command -v rbenv 1>/dev/null 2>&1; then
  eval "$(rbenv init - --no-rehash)"
fi

[[ -f "$HOME/.bun/_bun" ]] && source "$HOME/.bun/_bun"

[[ -f "$HOME/perl5/lib/perl5" ]] && eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"

# opam configuration
[[ ! -r $HOME/.opam/opam-init/init.zsh ]] || source $HOME/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
