(use-package ido-vertical-mode :ensure t :pin melpa
  :config
  (ido-vertical-mode 1))

(use-package ido
  :config
  (setq-default ido-use-faces t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (setq-default ido-max-prospects 6)
  (setq-default ido-enable-dot-prefix t)
  (setq-default ido-ignore-buffers
  '("\\` " "^\*\\(|Warning\\|Flycheck\\|Completions\\|Compile-Log\\)"))
  (ido-mode 1))

(use-package flx-ido :ensure t :pin melpa
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces t))
