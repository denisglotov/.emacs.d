#!/bin/sh -e
[ "$1" ] && EMACS_VERSION="$1"

check() {
    command -v $1 >/dev/null
}

echo "Installing emacs${EMACS_VERSION}..."
sudo apt-get install -y software-properties-common
if [ "$EMACS_VERSION" ]; then
    sudo add-apt-repository -y ppa:kelleyk/emacs
    sudo apt-get install -y emacs${EMACS_VERSION}-nox
else
    sudo apt-get update
    sudo apt-get install -y emacs-nox
fi
sudo apt-get install -y ispell

echo
if grep -qe '~/.emacs.d/profile.sh' ~/.bashrc; then
    echo "[Info] .bashrc already include our shell"
else
    echo "Appending our shell to .bashrc..."
    echo "source ~/.emacs.d/profile.sh" >>~/.bashrc
fi

echo
if [ "${EMACS_VERSION}" ]; then
    echo "Creating symbolic links..."
    sudo ln -sf emacs${EMACS_VERSION} /usr/bin/emacs
    sudo ln -sf emacsclient${EMACS_VERSION} /usr/bin/emacsclient
fi

echo
echo "All done."
emacs --version
