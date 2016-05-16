(require 'use-package)

(use-package js
  :config
  (setq-default js-indent-level 2))

(use-package js3-mode :ensure t :pin melpa
  :config
  (setq-default js-indent-level 2))

(use-package tern :ensure t :pin melpa
  :init
  (add-hook 'js3-mode-hook #'tern-mode)
  (add-hook 'js-mode-hook #'tern-mode))
