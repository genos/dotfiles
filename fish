set -g fish_key_bindings fish_vi_key_bindings
set -x PATH $PATH $HOME/bin $HOME/.local/bin $HOME/.cargo/bin
/opt/homebrew/bin/brew shellenv | source
zoxide init fish | source
direnv hook fish | source
pyenv init - fish | source
if status is-interactive
    # Commands to run in interactive sessions can go here
end
