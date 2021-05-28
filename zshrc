source $HOME/.zshenv
source $HOME/.zshprompt

# must come before compinit
fpath=(
  $HOME/site-functions
  $fpath
)
typeset -U fpath

autoload -Uz compinit
if [[  (-e $HOME/.zcompdump) && ($(date +'%j') != $(stat -f '%Sm' -t '%j' $HOME/.zcompdump)) ]]; then
  compinit
else
  compinit -C
fi

bindkey -v

path=(
  $HOME/bin
  $HOME/.local/bin
  $HOME/.cargo/bin
  $PYENV_ROOT/bin
  $GOPATH/bin
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

if [ $commands[fasd] ]; then # check if fasd is installed
  fasd_cache="$HOME/.fasd-init-cache"
  if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init auto >| "$fasd_cache"
  fi
  source "$fasd_cache"
  unset fasd_cache
fi

[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh

[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && source $HOME/.nix-profile/etc/profile.d/nix.sh

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi

[ -f $HOME/.poetry/env ] && source $HOME/.poetry/env

if command -v rbenv 1>/dev/null 2>&1; then
  eval "$(rbenv init -)"
fi

eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"
