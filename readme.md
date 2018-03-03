My emacs configuration.
=======================

Inspired by [Purcell emacs config][], although I keep it simple and
minimalistic.

[Purcell emacs config]: https://github.com/purcell/emacs.d


Tools
-----

Additional tools worth installing

Tool         |  Command                               |  Used by
------------ | -------------------------------------- | -----------------------
[Ispell][]   | `sudo apt-get install ispell`          | M-x ispell
[Solium][]   | `sudo npm install solium -g`           | fly-check .sol files
[golint][]   | `go get -u golang.org/x/lint/golint`   | M-x golint

[Ispell]: https://www.gnu.org/software/ispell/
[Solium]: https://github.com/duaraghav8/Solium
[golint]: https://github.com/golang/lint

Keys
----

Here are usefull key bindings that I sometimes forget :)

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


Happy coding 😺
