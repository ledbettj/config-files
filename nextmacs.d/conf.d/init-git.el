(use-package magit :ensure t :defer t
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))

(use-package diff-hl :ensure t :defer t)
