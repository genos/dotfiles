alias -- -='cd -'
alias -g ......='../../../../..'
alias -g .....='../../../..'
alias -g ....='../../..'
alias -g ...='../..'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'
alias a='fasd -a'
alias cadt='cat'
alias cp='nocorrect cp'
alias d='fasd -d'
alias e='$EDITOR'
alias f='fasd -f'
alias g='git'
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gca='git commit --all'
alias gcm='git checkout master'
alias gco='git checkout'
alias gd='git diff'
alias gl='git pull'
alias glol='git log --graph --decorate --oneline'
alias gm='git merge'
alias gp='git push'
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias gst='git status'
alias gwch='git whatchanged -p --abbrev-commit --pretty=medium'
alias history='fc -il 1'
alias ip='ipython'
alias ipn='jupyter notebook'
alias jc='$HOME/j901/bin/jconsole'
alias jqt='$HOME/j64-805/bin/jqt.command'
alias l='exa --git --header --long'
alias la='exa --all --git --header --long'
alias ln='nocorrect ln'
alias lrt='exa --long --sort=modified'
alias ls='ls -G'
alias md='mkdir -p'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias o='a -e open'
alias ocdb="psql 'postgresql://graham@pg06-21.mtv.quantifind.com:5432/opencorporates_db'"
alias py='python3'
alias qfTest="sbt/sbt 'project qfish' 'test:compile' 'test-only * -- -l \"com.qf.test.tags.IntegrationTest com.qf.test.tags.ManualTest com.qf.test.tags.IgnoreTest\"'"
alias rd='rmdir'
alias s='fasd -si'
alias sc='scalafmt -c $HOME/.scalafmt.conf'
alias scalariform='scalariform -p=$HOME/.scalariform.properties'
alias sd='fasd -sid'
alias sf='fasd -sif'
alias ssh='TERM=xterm-256color ssh'
alias spdb="psql 'postgresql://graham@dev-entitydb.mtv.quantifind.com:5432/sp_db'"
alias sp_dcteam="psql 'postgresql://graham@pg06-21.mtv.quantifind.com:5432/sp_dcteam'"
alias touch='nocorrect touch'
alias v='f -e $EDITOR'
alias whihc='which'
alias z='fasd_cd -d'
alias zz='fasd_cd -d -i'

bindkey "^X^I" expand-or-complete-prefix

export CPLUS_INCLUDE_PATH="$C_INCLUDE_PATH:$CPLUS_INCLUDE_PATH"
export C_INCLUDE_PATH="$HOME/include:$C_INCLUDE_PATH"
export DYLD_LIBRARY_PATH="/Developer/NVIDIA/CUDA-10.2/lib/${DYLD_LIBRARY_PATH:+:${DYLD_LIBRARY_PATH}}"
export EDITOR='vim'
export FZF_DEFAULT_COMMAND='rg --color never -g "" --files'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
export HISTSIZE=10000
export KEYTIMEOUT=1
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LD_LIBRARY_PATH="$HOME/lib:$LD_LIBRARY_PATH"
export LESS='CiMQRX'
export LSCOLORS="Gxfxcxdxbxegedabagacad"
export SAVEHIST=10000
export SHELL="/usr/local/bin/zsh"


function check_writing() {
  # Run the plaintext-ish file through various writing checks
  if [[ -n $1 ]]; then
    weasel $1 && passive $1 && dups $1 && alex $1
  else
    print "usage: $0 <file to check>"
  fi
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
  # Open up a nix environment with a specified GHC version and package
  # - http://alpmestan.com/posts/2017-09-06-quick-haskell-hacking-with-nix.html
  if [[ $# -lt 2 ]];
  then
    print "Must provide a ghc version (e.g ghc881) and at least one package"
    return 1;
  else
    ghcver=$1
    pkgs=${*:2}
    print "Starting haskell shell, ghc = $ghcver, pkgs = $pkgs"
    nix-shell -p "haskell.packages.$ghcver.ghcWithPackages (ps: with ps; [$pkgs])"
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

zstyle ':completion:*' list-colors "${(s.:.)LSCOLORS}"
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
