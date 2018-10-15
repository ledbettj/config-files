(use-package projectile :ensure t :defer t
  :bind (("C-c f" . projectile-find-file))
  :config
  (projectile-global-mode))
