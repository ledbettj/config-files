(use-package js
  :config
  (setq-default js-indent-level 2))

(use-package js3-mode :ensure t :pin melpa
  :config
  (setq-default js3-indent-level 2)
  (setq-default js3-indent-dots t)
  (setq-default js3-lazy-dots t)
  )

(use-package tern :ensure t :pin melpa
  :diminish " Ⓣ"
  :init
  (add-hook 'js3-mode-hook #'tern-mode)
  (add-hook 'js-mode-hook #'tern-mode))
