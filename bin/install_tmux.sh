#!/bin/bash -e

while [[ $# -gt 0 ]]; do
case $1 in
    -i|--ignore-existing)
        IGNORE=1
        echo "[Warning] Ignoring existing tmux if any."
        shift
        ;;
    -s|--skip-tmux-build)
        SKIP_BUILD=1
        echo "[Warning] Skipping tmux build"
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

check() {
    command -v $1 >/dev/null
}

libcheck() {
    ldconfig -p | grep libjpeg >/dev/null
}

if [ -z "$SKIP_BUILD" ]; then
    [ ! -v IGNORE ] && check tmux && die "[Error] $(tmux -V) already installed."

    if check aclocal && check yacc && check automake; then
        echo "[Info] All build tools are installed."
    else
        echo "Need to install build tools with sudo..."
        sudo apt install autotools-dev automake bison
    fi

    if libcheck libevent && libcheck libncurses; then
        echo "[Info] All libraries are installed."
    else
        echo "Need to install libraries with sudo..."
        sudo apt install libevent-dev libncurses-dev
    fi

    echo
    echo "Installing tmux $TMUX_TAG..."
    [ -d /tmp/tmux ] || git clone https://github.com/tmux/tmux.git /tmp/tmux
    cd /tmp/tmux
    git checkout "$TMUX_TAG"

    echo
    echo "Building tmux..."
    sh autogen.sh
    ./configure --prefix=$HOME && make && make install
fi

echo
echo "Installing tmux plugins..."
[ -d ~/.tmux/plugins/tpm ] ||
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

echo
echo "Composing .tmux.config..."
if [ -f ~/.tmux.conf ]; then
    echo "[Warning] Backing up .tmux.conf here..." &&
    mv ~/.tmux.conf ~/.tmux.conf.old
fi
cat >~/.tmux.conf <<EOF
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-resurrect'

set -g default-terminal "screen-24bit-256color"
set -g terminal-overrides ",xterm-256color:Tc"
set -g mouse on

set -g status-fg colour006
set -g status-bg blue

set -g @resurrect-save-shell-history 'on'

# Initialize TMUX plugin manager (keep this line at the very bottom of tmux.conf)
run -b '~/.tmux/plugins/tpm/tpm'
EOF

echo
echo "All set: $($HOME/bin/tmux -V)"
