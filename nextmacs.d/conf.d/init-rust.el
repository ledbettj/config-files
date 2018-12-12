(use-package rust-mode :ensure t :defer t
  :hook (rust-mode . (lambda ()
                       (lsp-rust-enable)
                       (lsp-mode)
                       (lsp-ui-mode)
                       (lsp-ui-sideline-mode)
                       (lsp-ui-doc-mode))))

(use-package toml-mode :ensure t :defer t)

(use-package lsp-rust :ensure t
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))
