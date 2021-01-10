(use-package scala-mode
  :ensure t
  :defer t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :ensure t
  :defer t
  :hook ((java-mode scala-mode) . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :defer t)

(use-package lsp-java
  :ensure t
  :defer t
  :config (add-hook 'java-mode-hook 'lsp))

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet
  :ensure t
  :defer t)

;; Add company-lsp backend for metals
(use-package company-lsp
  :ensure t
  :defer t)

(provide 'init-scala)
