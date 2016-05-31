(use-package flyspell :ensure t :pin melpa
  :diminish " ⓕ"
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))
  (setq flyspell-auto-correct-binding (kbd "C-'")))
