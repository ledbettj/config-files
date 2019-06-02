(use-package lsp-mode :ensure t
  :commands lsp
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))
(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :init
  (setq-default lsp-ui-doc-use-webkit t))
