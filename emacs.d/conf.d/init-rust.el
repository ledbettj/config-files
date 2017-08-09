(use-package rust-mode :ensure t :pin melpa
  :config
  (setq-default rust-indent-offset 2))

(use-package cargo :ensure t :pin melpa
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package racer :ensure nil :pin melpa
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :config
  (setq racer-cmd (expand-file-name "~/.multirust/toolchains/stable/cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/Projects/rust/rustc-1.8.0/src")))

(use-package toml-mode :ensure t :pin melpa)
