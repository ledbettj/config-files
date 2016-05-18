(use-package magit :ensure t :pin melpa
  :bind (("C-c g" . magit-status)
         ("C-c b" . magit-blame)))

(use-package diff-hl :ensure t :pin melpa
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  :config
  (setq-default diff-hl-draw-borders nil)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
