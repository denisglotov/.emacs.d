(use-package solidity-mode
  :ensure t
  :defer t
  :config
  (use-package solidity-flycheck :ensure t)
  (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-solium-checker-active nil)
  ;;(setq-default solidity-solc-path "~/.emacs.d/bin/solc-solium-wrapper.sh")
  (setq c-basic-offset 4)
)

(provide 'init-solidity)
