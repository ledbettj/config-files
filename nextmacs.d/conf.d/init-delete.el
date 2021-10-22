(use-package hungry-delete :ensure t :defer t
  :config
  (setq hungry-delete-chars-to-skip " \t")
  :init
  (add-hook 'minibuffer-setup-hook (lambda () (hungry-delete-mode -1)))
  (global-hungry-delete-mode))
