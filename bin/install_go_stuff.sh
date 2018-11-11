#! /bin/sh -x
#
# Go handy stuff. Taken from http://reangdblog.blogspot.com/2016/06/emacs-ide-go.html.
#

# For Flycheck
go get -u github.com/golang/lint/golint
go get -u github.com/kisielk/errcheck

# For code navigation
go get -u golang.org/x/tools/cmd/guru
go get -u github.com/rogpeppe/godef

# Autocompletion and refactoring
go get -u github.com/mdempsky/gocode
go get -u golang.org/x/tools/cmd/gorename
go get -u golang.org/x/tools/cmd/goimports

# go get -u github.com/jstemmer/gotags
# sudo go get -u golang.org/x/tools/cmd/godoc
