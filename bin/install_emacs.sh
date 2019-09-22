#!/bin/sh
[ "$1" ] && EMACS_VERSION="$1" || EMACS_VERSION="26"

check() {
    command -v $1 >/dev/null
}

echo "Installing emacs${EMACS_VERSION}..."
sudo add-apt-repository -y ppa:kelleyk/emacs
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
echo "All done: $(emacs${EMACS_VERSION} --version)."
