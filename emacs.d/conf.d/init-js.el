(use-package js
  :init
  (add-hook 'js-mode-hook '(lambda () (setq mode-name "Js")))
  :config
  (setq-default js-indent-level 2))

(use-package tern :ensure t :pin melpa
  :diminish " Ⓣ"
  :init
  (add-hook 'js-mode-hook #'tern-mode))
