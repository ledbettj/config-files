(use-package hungry-delete :ensure t :pin melpa
  :diminish " ⓗ"
  :init
  (add-hook 'c-mode-hook
            '(lambda ()
               (turn-on-hungry-delete-mode)
               (local-set-key (kbd "<backspace>") 'hungry-delete-backward)
               ))
  :config
  (setq hungry-delete-chars-to-skip " \t\r\f\v")
  (global-hungry-delete-mode))
