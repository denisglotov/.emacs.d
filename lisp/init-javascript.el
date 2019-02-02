;; Configuration for JS.
(require 'js2-mode)
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; use local prettier from node_modules before global one
(defvar my/prettier nil)
(defun my/use-prettier-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                (lambda (dir)
                 (file-executable-p (expand-file-name "node_modules/.bin/prettier" dir)))))
         (binary (expand-file-name "node_modules/.bin/prettier" root)))
    (when (file-executable-p binary) (setq-local my/prettier binary))))

;; use local eslint from node_modules before global one
(defun my/use-eslint-from-node-modules ()
  ;; (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                (lambda (dir)
                 (file-executable-p (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))))
         (eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
    ;; (message eslint)
    (when (file-executable-p eslint) (setq-local flycheck-javascript-eslint-executable eslint))))

(defun prettify ()
  (interactive)
  (if my/prettier
      (progn
        (shell-command
         (format "%s --write %s"
                 my/prettier
                 (shell-quote-argument (expand-file-name buffer-file-name))))
        (revert-buffer t t t))
    (message "Prettier not found for this buffer")))
(global-set-key (kbd "C-c C-p") 'prettify)

(with-eval-after-load 'js2-mode
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil
                js2-strict-trailing-comma-warning nil)
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook #'my/use-prettier-from-node-modules)

  ;; name the mode more concise
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2"))))

(provide 'init-javascript)
