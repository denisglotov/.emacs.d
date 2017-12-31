;; Explicitly require what needs to be.
(require 'flycheck)

(setq-default solidity-flycheck-solc-checker-active t)
(setq-default solidity-flycheck-solium-checker-active nil)
(setq-default solidity-solc-path "~/.emacs.d/bin/solc-solium-wrapper.sh")
(require 'solidity-mode)

(with-eval-after-load 'solidity-mode
  (setq-default c-basic-offset 4))

(provide 'init-solidity)
