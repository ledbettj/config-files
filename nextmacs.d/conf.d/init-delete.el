(use-package hungry-delete :ensure t :defer t
  :config
  (setq hungry-delete-chars-to-skip " \t")
  :init
  (global-hungry-delete-mode))
