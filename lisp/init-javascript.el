;; Configuration for JS.
(require 'js2-mode)
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; use local eslint from node_modules before global one
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(with-eval-after-load 'js2-mode
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil
                js2-strict-trailing-comma-warning nil)

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; name the mode more concise
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2"))))

(provide 'init-javascript)
