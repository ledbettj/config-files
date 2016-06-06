(use-package flyspell :ensure t :pin melpa
  :diminish " ⓕ"
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook  #'flyspell-mode)
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))
  (setq flyspell-auto-correct-binding (kbd "C-'")))
