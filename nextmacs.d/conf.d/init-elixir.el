(use-package elixir-mode :ensure t
  :config
  (setq-default rust-indent-offset 2)
  :hook (elixir-mode . (lambda ()
                         (lsp)
                         (lsp-ui-mode)
                         (lsp-ui-sideline-mode))))

