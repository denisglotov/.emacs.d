#!/bin/bash -e

while [[ $# -gt 0 ]]; do
case $1 in
    -i|--ignore-existing)
        IGNORE=1
        echo "[Warning] Ignoring existing tmux if any."
        shift
        ;;
    --)
        shift
        ;;
esac
done

[ "$1" ] && TMUX_TAG="$1" || TMUX_TAG="2.9a"

die() {
    echo $1 >&2
    exit 1
}

[ ! -v IGNORE ] && command -v tmux >/dev/null && die "[Error] $(tmux -V) already installed."

echo "Installing tmux $TMUX_TAG..."
[ -d /tmp/tmux ] || git clone https://github.com/tmux/tmux.git /tmp/tmux
cd /tmp/tmux
git checkout "$TMUX_TAG"

echo
echo "Building tmux..."
# Needs aclocal, yacc, automake, autoreconf, libevent
sudo apt install autotools-dev automake bison libevent-dev libncurses-dev
sh autogen.sh
./configure --prefix=$HOME && make && make install


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
