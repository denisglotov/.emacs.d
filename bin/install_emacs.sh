#!/bin/sh

if ! command -v emacs >/dev/null; then
    echo "Installing emacs..."
    sudo apt-get install emacs-nox
fi

command -v ispell >/dev/null || sudo apt-get install ispell

if ! grep -qe '~/.emacs.d/profile.sh' ~/.bashrc; then
    echo "Appending our shell to .bashrc..."
    echo "source ~/.emacs.d/profile.sh" >>~/.bashrc
fi

echo "All done."
