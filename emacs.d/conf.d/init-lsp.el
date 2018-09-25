(use-package lsp-mode :ensure t
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls")))
(use-package lsp-ui :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-rust :ensure t)
