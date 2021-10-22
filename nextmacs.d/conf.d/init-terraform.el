
(use-package terraform-mode :ensure t :defer t
  :hook (terraform-mode . (lambda () (lsp))))

