(use-package lsp-mode :ensure t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-rust-server 'rust-analyzer))
(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :init
  (setq-default lsp-ui-flycheck-enable nil)
  (setq-default lsp-prefer-flymake t)
  (setq-default lsp-ui-doc-max-height 5))
