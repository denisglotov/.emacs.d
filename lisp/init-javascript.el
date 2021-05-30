;; Configuration for JS.
(require 'flycheck)

(use-package js2-mode
  :mode "\\.js\\'"
  :ensure t
  :preface
  ;; use local prettier from node_modules before global one
  (defvar my/prettier nil)
  (defun my/use-prettier-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  (lambda (dir)
                    (file-executable-p
                     (expand-file-name "node_modules/.bin/prettier" dir)))))
           (binary (expand-file-name "node_modules/.bin/prettier" root)))
      (when (file-executable-p binary) (setq-local my/prettier binary))))

  ;; use local eslint from node_modules before global one
  (defun my/use-eslint-from-node-modules ()
    ;; (interactive)
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  (lambda (dir)
                    (file-executable-p
                     (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))))
           (eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
      ;; (message eslint)
      (when (file-executable-p eslint)
        (setq-local flycheck-javascript-eslint-executable eslint))))

  ;; prettify buffer
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

  :bind ("C-c C-p" . prettify)

  :config
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil
                js2-strict-trailing-comma-warning nil)

  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)

  ;; disable jshint since we prefer eslint checking
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook #'my/use-prettier-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1)

  ;; name the mode more concise
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  )

;; needs apt-get install silversearcher-ag
(use-package xref-js2
  :after js2-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package typescript-mode
  :mode "\\.js\\'"
  :ensure t
  :config
  (setq-default typescript-indent-level 2)
)

(provide 'init-javascript)
