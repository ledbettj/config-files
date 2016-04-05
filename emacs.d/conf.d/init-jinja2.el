(require 'use-package)

(use-package jinja2-mode :ensure t :pin melpa
  :init
  (add-to-list 'auto-mode-alist '("\\.j2\\'"  . jinja2-mode)))
