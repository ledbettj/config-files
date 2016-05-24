(use-package flyspell :ensure t :pin melpa
  :diminish " ⓕ"
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq flyspell-auto-correct-binding (kbd "C-'")))
