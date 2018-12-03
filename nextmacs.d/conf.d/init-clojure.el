(use-package rainbow-delimiters :ensure t)

(use-package clojure-mode :ensure t
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider :ensure t
  :bind (
         :map clojure-mode-map
         ("C-c j" . cider-jack-in)
         ("C-c d" . cider-doc))
  :config
  ;; don't prompt to save on C-c C-k to recompile.
  (setq cider-prompt-save-file-on-load 'always-save)
  ;; hide the => prefix when evaluating inline
  (setq cider-interactive-eval-result-prefix ""))
