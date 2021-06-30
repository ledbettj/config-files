(use-package rspec-mode :ensure t)
(use-package yard-mode :ensure t
  :hook ruby-mode)

(use-package yaml-mode :ensure t)

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil))
