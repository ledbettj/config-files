(require 'use-package)

(use-package magit :ensure t :pin melpa
  :bind (("C-c g" . magit-status)
         ("C-c b" . magit-blame)))
