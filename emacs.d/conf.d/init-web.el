(use-package web-mode :ensure t :pin melpa
  :init
  (add-to-list 'auto-mode-alist '("\\.erb\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2))
