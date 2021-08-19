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
