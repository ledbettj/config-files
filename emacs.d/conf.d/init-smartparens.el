(use-package smartparens :ensure t :pin melpa
  :diminish " Ⓢ"
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (setq-default sp-autoescape-string-quote nil)
  (setq-default sp-highlight-pair-overlay  nil)
  (require 'smartparens-config))
