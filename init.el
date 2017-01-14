(setq my-packages '(
                    js2-mode
                    markdown-mode
                    tool-bar\+
                    whitespace-cleanup-mode
                    web-mode
                    ))

(require 'package)
(package-initialize)
(add-to-list 'package-archives `("melpa" . "https://melpa.org/packages/"))
;; Note: In case of MELPA problems, the official mirror URL is
;; https://www.mirrorservice.org/sites/stable.melpa.org/packages/

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

(require 'cl-lib)
(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `my-packages'.  Useful for
  cleaning out unwanted packages."
    (interactive)
    (package-show-package-list
     (remove-if-not (lambda (x) (and (not (memq x my-packages))
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
    (setq mouse-sel-mode t)

    (menu-bar-mode -1)  ; hide menu
    ))

(add-hook 'after-make-frame-functions
          'mouse-support-in-term t)
(mouse-support-in-term nil)

;; Tabbar...
(setq tabbar-ruler-global-tabbar t)  ; If you want tabbar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; disable toolbar

;; Backup dirs.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
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

;; Basic defaults.
(setq-default
 column-number-mode t
 indent-tabs-mode nil
 scroll-preserve-screen-position 'always
 fill-column 78
 truncate-lines nil)

;; Whitespaces.
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

;; Show matching parens.
(show-paren-mode 1)

;; save a list of open files in ~/.emacs.d/.emacs.desktop.
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

;; Allow access from emacsclient.
(require 'server)
(unless (server-running-p)
    (server-start))

(provide 'init)
