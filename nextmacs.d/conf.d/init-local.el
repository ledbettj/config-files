(use-package sudo
  :load-path "local/"
  :bind (("C-c C-s" . reopen-file-with-sudo)))

(use-package magic-align
  :load-path "local/"
  :bind (("C-c a" . magic-align)))

(use-package move-dup
  :load-path "local/"
  :bind (([M-up] . md/move-lines-up)
         ([M-down] . md/move-lines-down)))

(use-package newcomment
  :bind (("C-c c" . comment-region)
         ("C-c u" . uncomment-region)))

(use-package copilot
  :load-path "local/copilot.el/"
  :hook (prog-mode . copilot-mode)
  (global-copilot-mode)
  :bind (:map copilot-mode-map
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("C-M-<return>" . copilot-accept-completion))
  :diminish)
