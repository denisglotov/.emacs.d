alias e="pgrep emacs >/dev/null && emacsclient -nw || emacs --daemon"
export EDITOR="emacsclient -nw -a nano"
export VISUAL="emacsclient -c -a nano"
