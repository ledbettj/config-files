(use-package smartparens :ensure t :pin melpa
  :diminish " Ⓢ"
  :init
  (add-hook 'prog-mode-hook '(lambda ()
                               (unless (eq major-mode 'web-mode)
                                 (smartparens-mode t))))
  :config
  (setq-default sp-autoescape-string-quote nil)
  (setq-default sp-highlight-pair-overlay  nil)
  (require 'smartparens-config))
