#!/bin/bash -e

TMUX_TAG="$1" || "2.9a"

if command -v tmux >/dev/null; then
    echo "[Warning] $(tmux -V) already installed. Skipping."
else
    echo "Installing tmux $TMUX_TAG..."
    git clone https://github.com/tmux/tmux.git /tmp/tmux
    cd /tmp/tmux
    git checkout "$TMUX_TAG"

    echo
    echo "Building tmux..."
    # Needs aclocal, yacc, automake, autoreconf
    sudo apt install autotools-dev automake bison
    sh autogen.sh
    ./configure && make
    [ -d ~/bin ] || mkdir ~/bin
    cp tmux ~/bin/tmux
fi

# git clone https://github.com/tmux-plugins/tmux-resurrect.git ~/tmux-resurrect

# [ -f ~/.tmux.conf ] &&
#     echo "(!) Backing up .tmux.conf here..." &&
#     mv ~/.tmux.conf .tmux.conf.old

# cat >~/.tmux.conf <<EOF
# set -g default-terminal "screen-256color"
# set -g mouse on

# set -g status-fg colour006
# set -g status-bg blue

# set -g @resurrect-save-shell-history 'on'
# run-shell ~/tmux-resurrect/resurrect.tmux
# EOF

echo
~/bin/tmux -V
echo "All done."
