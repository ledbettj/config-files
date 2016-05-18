(use-package rust-mode :ensure t :pin melpa
  :config
  (setq-default rust-indent-offset 2))

(use-package cargo :ensure t :pin melpa
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package racer :ensure t :pin melpa
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "FIXME"))
