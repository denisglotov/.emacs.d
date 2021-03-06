#!/bin/bash -e
help() {
    echo "Usage: $0 [-s|--skip-tmux-build [-i|--ignore-existing]]"
    echo "       [-c|--color COLOR] [-t|--tag TMUX_GIT_TAG]"
    exit 0
}

die() {
    echo $1 >&2
    exit 1
}

check() {
    command -v $1 >/dev/null
}

pkgcheck() {
    apt list --installed | grep $1 >/dev/null
}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
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
    -c|--color)
        COLOR=$2
        shift
        shift
        ;;
    -t|--tag)
        TMUX_TAG=$2
        shift
        shift
        ;;
    -h|--help)
        help
        ;;
    --)
        shift
        ;;
esac
done
[ "$TMUX_TAG" ] || TMUX_TAG="2.9a"

if [ -z "$SKIP_BUILD" ]; then
    [ ! -v IGNORE ] && check tmux && die "[Error] $(tmux -V) already installed."

    if pkgcheck build-essential && check aclocal && check yacc && check automake; then
        echo "[Info] All build tools are installed."
    else
        echo "Need to install build tools with sudo..."
        sudo apt install build-essential autotools-dev automake bison
    fi

    if pkgcheck libevent-dev && pkgcheck libncurses-dev; then
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
    ./configure --prefix=$HOME
    make
    make install
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
[ "$COLOR" ] || COLOR=$(cat /etc/ssh/ssh_host_*_key.pub | ${DIR}/calculate_color.py)
echo "[Info] with background color ${color}."
cat >~/.tmux.conf <<EOF
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-resurrect'

set -g default-terminal "screen-256color"
set -g terminal-overrides ",xterm-256color:Tc"
set -g mouse on

set -g status-fg colour006
set -g status-bg "${COLOR}"

set -g @resurrect-save-shell-history 'on'
set-environment -g TMUX_PLUGIN_MANAGER_PATH '~/.tmux/plugins/'

# Initialize TMUX plugin manager (keep this line at the very bottom of tmux.conf)
run -b '~/.tmux/plugins/tpm/tpm'
EOF

echo
echo "Install plugins..."
~/.tmux/plugins/tpm/bin/install_plugins

echo
echo "All done"
