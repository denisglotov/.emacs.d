My Emacs configuration
======================

[![Build status]](https://travis-ci.org/denisglotov/.emacs.d)

[Build status]: https://travis-ci.org/denisglotov/.emacs.d.svg?branch=master

And Tmux too. And scripts to install this all to a new host.

Good emacs configurations that I took inspiration from:

* Purcell emacs config: https://github.com/purcell/emacs.d/
* John Wigley emacs: https://github.com/jwiegley/dot-emacs/


Scripts
-------

[bin/install_emacs.sh] - installs emacs (-nox version) of the specified
version,

[bin/install_tmux.sh] - build from repo or install tmux and its config,

[bin/calculate_color.py] - calculate some dark color in a form "#rrggbb"
deterministically based on argument, used to pick host-unique color for tmux
status line,


[bin/install_emacs.sh]: bin/install_emacs.sh
[bin/install_tmux.sh]: bin/install_tmux.sh
[bin/calculate_color.py]: bin/calculate_color.py


Tools
-----

Specific tools needed for developing in some language are installed by the
corresponding [bin](bin/) installer.

For Go, see http://golang.org/s/using-guru.

Additional handy tools (not yet sorted):

Tool         |  Command                               |  Used by
------------ | -------------------------------------- | -----------------------
[Ispell][]   | `sudo apt-get install ispell`          | `M-x ispell`
[Solium][]   | `sudo npm install solium -g`           | flycheck Solidity code

[Ispell]: https://www.gnu.org/software/ispell/
[Solium]: https://github.com/duaraghav8/Solium


Keys
----

Here are useful key bindings that I sometimes forget :)

Keys    | Command                 | Description
------- | ----------------------- | -------------------------------------------
C-M-n   | forward-list            | Move forward over a parenthetical group
C-M-p   | backward-list           | Move backward over a parenthetical group
C-M-u   | backward-up-list        | Move up in parenthesis structure
C-M-d   | down-list               | Move up in parenthesis structure
C-M-f   | forward-sexp            | Move forward over a balanced expression
C-M-b   | backward-sexp           | Move backward over a balanced expression
C-M-k   | kill-sexp               | Kill balanced expression forward
C-M-SPC | mark-sexp               | Put the mark at the end of the sexp
C-x (   |                         | Start defining a keyboard macro
C-x )   |                         | Stop defining a keyboard macro
C-x e   |                         | Execute the keyboard macro (C-u 0 C-x e)
---     | name-last-kbd-macro     | Name the last-defined keyboard macro
---     | insert-kbd-macro        | Insert a named keyboard macro at point
C-c ! l | flycheck-list-errors    | Pop up a list of all errors in current buffer
C-c ! n | flycheck-next-error     |
C-c ! p | flycheck-previous-error |
C-x SPC |                         | Rectangle mode selection
C-x TAB |                         | Indent region left-right

Happy coding 😺
