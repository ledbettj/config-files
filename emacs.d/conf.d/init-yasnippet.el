(require 'use-package)

(use-package yasnippet :ensure t :pin melpa
  :diminish yas-minor-mode
  :config
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))
