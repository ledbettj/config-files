(require 'use-package)

(use-package js-mode
  :config
  (setq-default js-indent-level 2))

(use-package tern :ensure t :pin melpa
  :init
  (add-hook 'js-mode-hook #'tern-mode))
