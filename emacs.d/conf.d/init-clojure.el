(require 'use-package)

(use-package rainbow-delimiters :ensure t :pin melpa)

(use-package cider :ensure t :pin melpa
  :bind (("C-c d" . cider-doc))
  :config
  ;; don't prompt to save on C-c C-k to recompile.
  (setq cider-prompt-save-file-on-load 'always-save)
  ;; hide the => prefix when evaluating inline
  (setq cider-interactive-eval-result-prefix ""))

(use-package clojure-mode :ensure t :pin melpa
  :bind (
         :map clojure-mode-map
         ("C-c j" . cider-jack-in))
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
