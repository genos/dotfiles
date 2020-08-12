source $HOME/.zshprompt
source $HOME/.zshenv
source $HOME/SOURCE_ME

autoload -Uz compinit
if [[  (-e $HOME/.zcompdump) && ($(date +'%j') != $(stat -f '%Sm' -t '%j' $HOME/.zcompdump)) ]]; then
  compinit
else
  compinit -C
fi
bindkey -v
fpath=(
  $HOME/site-functions
  $fpath
)
typeset -U fpath

path=(
  $HOME/bin
  $HOME/.local/bin
  /Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /usr/sbin
  /bin
  /sbin
  /Library/TeX/Distributions/Programs/texbin
  $HOME/.cargo/bin
  /usr/local/opt/python@3/bin
  $GOPATH/bin
  $HOME/.poetry/bin
  $path
)
typeset -U path

[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && source $HOME/.nix-profile/etc/profile.d/nix.sh

if [ $commands[fasd] ]; then # check if fasd is installed
  fasd_cache="$HOME/.fasd-init-cache"
  if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init auto >| "$fasd_cache"
  fi
  source "$fasd_cache"
  unset fasd_cache
fi

source $HOME/._tmuxinator

[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh

eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
