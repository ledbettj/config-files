(use-package rust-mode :ensure t :defer t
  :config
  (setq-default rust-indent-offset 2)
  :hook (rust-mode . (lambda ()
                       (lsp)
                       (lsp-ui-mode)
                       (lsp-ui-sideline-mode))))

(use-package toml-mode :ensure t :defer t)
