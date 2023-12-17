;; https://rust-analyzer.github.io/manual.html#installation
(use-package rustic
  :ensure
  :after (lsp-mode lsp-ui)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("M-n" . flycheck-goto-next-error)
              ("M-p" . flycheck-goto-prev-error)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :custom
  (lsp-rust-analyzer-cargo-watch-enable nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :config

  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;;(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

;; (use-package rust-mode
;;   :ensure t
;;   :bind (
;;          ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
;;          ;; uncomment the following lines
;;          ;; ("C-c C-j" . lsp-find-definition)
;;          ;; ("C-c C-d" . lsp-describe-thing-at-point)
;;          )
;;   :hook ((rust-mode-hook . lsp-deferred)))

(provide 'init-rust)
