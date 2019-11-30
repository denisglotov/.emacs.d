;; Configuration for Python.
(require 'cc-mode)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(provide 'init-python)
