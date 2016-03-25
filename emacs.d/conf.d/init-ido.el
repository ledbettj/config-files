(require 'use-package)

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
  '("\\` " "^\*\\(Messages\\|Warning\\|Flycheck\\|Completions\\)"))
  (ido-mode 1))
