#!/bin/bash

git clone https://github.com/tmux-plugins/tmux-resurrect.git ~/tmux-resurrect

[ -f ~/.tmux.conf ] &&
    echo "(!) Backing up .tmux.conf here..." &&
    mv ~/.tmux.conf .tmux.conf.old

cat >~/.tmux.conf <<EOF
set -g default-terminal "screen-256color"
set -g mouse on

set -g status-fg colour006
set -g status-bg blue

set -g @resurrect-save-shell-history 'on'
run-shell ~/tmux-resurrect/resurrect.tmux
EOF

if ! command -v tmux >/dev/null; then
    echo "Installing tmux..."
    sudo apt-get install tmux
fi

echo "All done."
