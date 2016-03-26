(require 'use-package)

(use-package rust-mode :ensure t :pin melpa
  :config
  (setq-default rust-indent-offset 2))
