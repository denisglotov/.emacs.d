;;; init.el --- Initialization file for Emacs

(defvar my-packages
  '(
    go-autocomplete
    go-eldoc
    go-guru
    go-mode
    golint
    js2-mode
    solidity-mode
    use-package
    )
  )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install any packages in my-packages, if they are not installed already.
(let ((refreshed nil))
  (when (not package-archive-contents)
    (package-refresh-contents)
    (setq refreshed t))
  (dolist (pkg my-packages)
    (when (and (not (package-installed-p pkg))
               (assoc pkg package-archive-contents))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

(require 'use-package)

(require 'cl-lib)
(defun package-list-unaccounted-packages ()
  "Show only the packages that are installed and are not in
  my-packages list. Useful for cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (memq x my-packages))
                                      (not (package-built-in-p x))
                                      (package-installed-p x)))
                     (mapcar 'car package-archive-contents))))

;; Enable mouse support.
(defun mouse-support-in-term (frame)
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda ()
                                (interactive)
                                (scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
                                (interactive)
                                (scroll-up 1)))
    (defun track-mouse (e))

    (menu-bar-mode -1)  ; hide menu
    ))

(add-hook 'after-make-frame-functions
          'mouse-support-in-term t)
(mouse-support-in-term nil)

;; Tabbar...
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; disable toolbar

;; Backup dirs.
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup" user-emacs-directory))))
(message "%s" backup-directory-alist)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

;; Custom keys mapping.
(global-set-key [C-up] (lambda () (interactive) (scroll-up 1)) )
(global-set-key [C-down] (lambda () (interactive) (scroll-down 1)) )

;; Be like Mainframe.
(global-set-key (kbd "<f7>") (lambda () (interactive) (scroll-down 16)) )
(global-set-key (kbd "<f8>") (lambda () (interactive) (scroll-up 16)) )

;; Other usefulness.
(global-set-key (kbd "<f5>") 'revert-all-buffers)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
(global-set-key [remap just-one-space] 'cycle-spacing)
(global-set-key (kbd "M-l") 'copy-current-line-position-to-clipboard)

;; Basic defaults.
(setq-default
 column-number-mode t
 indent-tabs-mode nil
 scroll-preserve-screen-position 'always
 fill-column 78
 truncate-lines nil
 ring-bell-function 'ignore)

;; Show matching parens.
(show-paren-mode 1)
(ido-mode 1)

;; save a list of open files in ~/.emacs.d/.emacs.desktop.
(require 'desktop)
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(use-package flycheck
  :ensure t
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Load additional configs.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-go)
(require 'init-java)
(require 'init-javascript)
(require 'init-python)
(require 'init-solidity)

(use-package compile
  :no-require
  :bind ("C-c c" . compile)
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :preface
  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook (compilation-filter . compilation-ansi-color-process-output))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package ggtags
  :disabled t
  :commands ggtags-mode
  :diminish)

;; Allow access from emacsclient.
(use-package server
  :unless noninteractive
  :no-require
  :hook (after-init . server-start))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package json-reformat
  :ensure t
  :after json-mode)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("readme\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :ensure t
  :commands web-mode)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish
  :commands whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(message "All done, happy hacking 😺")
(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elpy meghanada whitespace-cleanup-mode yasnippet web-mode use-package solidity-mode s pyvenv markdown-mode json-mode js2-mode highlight-indentation golint go-guru go-eldoc go-autocomplete flycheck find-file-in-project docker-compose-mode company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
