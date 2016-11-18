(use-package yaml-mode :ensure t :pin melpa
  :config
  (add-hook 'yaml-mode-hook '(lambda () (flyspell-mode-off))))
