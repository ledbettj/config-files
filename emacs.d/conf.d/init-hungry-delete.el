(use-package hungry-delete :ensure t :pin melpa
  :diminish " ⓗ"
  :config
  (setq hungry-delete-chars-to-skip " \t\r\f\v")
  (global-hungry-delete-mode))
