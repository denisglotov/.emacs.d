#!/bin/sh -e
[ "$1" ] && EMACS_VERSION="$1" || EMACS_VERSION="26"

check() {
    command -v $1 >/dev/null
}

echo "Installing emacs${EMACS_VERSION}..."
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get install emacs${EMACS_VERSION}-nox
sudo apt-get install ispell

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
