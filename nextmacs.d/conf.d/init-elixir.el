(use-package elixir-mode :ensure t
  :config
  (setq-default elixir-basic-offset 2)
  :hook (elixir-mode . (lambda ()
                         (lsp)
                         (lsp-ui-mode)
                         (lsp-ui-sideline-mode))))

