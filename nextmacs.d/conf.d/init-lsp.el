(use-package lsp-mode :ensure t
  :commands lsp
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :init
  (setq-default lsp-ui-doc-max-height 5))
(use-package company-lsp :ensure t :commands company-lsp)
