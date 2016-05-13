(require 'use-package)

(use-package yasnippet :ensure t :pin melpa
  :config
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))
