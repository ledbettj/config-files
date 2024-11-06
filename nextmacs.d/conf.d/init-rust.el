(use-package rustic :ensure t :defer t
  :config
  (setq-default rust-indent-offset 2)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :hook (rust-mode . (lambda ()
                       (lsp)
                       (lsp-ui-mode)
                       (lsp-ui-sideline-mode))))

(use-package toml-mode :ensure t :defer t)
