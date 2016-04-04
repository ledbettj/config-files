(require 'use-package)

(use-package rainbow-delimiters :ensure t :pin melpa)

(use-package cider :ensure t :pin melpa)

(use-package clojure-mode :ensure t :pin melpa
  :bind (
         :map clojure-mode-map
         ("C-c j" . cider-jack-in))
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
