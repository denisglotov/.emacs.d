;; Install with `go get -u golang.org/x/lint/golint`.
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
