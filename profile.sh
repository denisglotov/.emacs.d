#!/bin/bash

EMACS_CMD=(
    "pgrep emacs >/dev/null ||"
    "( rm ~/.emacs.d/.emacs.desktop.lock 2>/dev/null; emacs --daemon );" \
    "emacsclient -nw" )
alias e="${EMACS_CMD[@]}"
export EDITOR="emacsclient -nw -a nano"
export VISUAL="emacsclient -c -a nano"
export GOPATH=$HOME

if [ ! -S ~/.ssh/ssh_auth_sock ]; then
  eval `ssh-agent`
  ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock
ssh-add -l > /dev/null || ssh-add
