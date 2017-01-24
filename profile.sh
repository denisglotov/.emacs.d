alias e="pgrep emacs >/dev/null || ( rm ~/.emacs.d/.emacs.desktop.lock; emacs --daemon ); emacsclient -nw"
export EDITOR="emacsclient -nw -a nano"
export VISUAL="emacsclient -c -a nano"
