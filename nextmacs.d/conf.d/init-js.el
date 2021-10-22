(use-package js
  :mode (("\\.js\\'" . js-mode)
         ("\\.json" . js-mode))
  :config
  (setq-default js-indent-level 2))

(use-package rjsx-mode :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.jsx\\'")
  :hook (rjsx-mode . (lambda () (lsp))))

