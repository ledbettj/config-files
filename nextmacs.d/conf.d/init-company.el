
(use-package company :ensure t :defer t
  :init (global-company-mode)
  :config
  (setq-default company-idle-delay                0.25)
  (setq-default company-minimum-prefix-length     2)
  (setq-default company-show-numbers              1)
  (setq-default company-tooltip-align-annotations t))

(use-package company-lsp :ensure t :commands company-lsp)
(use-package company-terraform :ensure t)
