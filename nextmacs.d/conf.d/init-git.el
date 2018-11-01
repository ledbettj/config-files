(use-package magit :ensure t :defer t
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))

(use-package diff-hl :ensure t :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  :config
  (set-face-background 'diff-hl-insert "#679A01")
  (set-face-background 'diff-hl-change "#1DB4D0")
  (set-face-background 'diff-hl-delete "#F20055")
  (setq-default diff-hl-draw-borders nil))
