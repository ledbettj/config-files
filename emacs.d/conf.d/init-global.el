(require 'use-package)

(global-set-key (kbd "M-g") 'goto-line)

(use-package hungry-delete
  :load-path "local/"
  :bind (([remap backward-delete-char-untabify] . hungry-delete-backwards)))

(use-package move-text :ensure t :pin melpa
  :bind (([M-up] . move-text-up)
         ([M-down] . move-text-down)))

(use-package newcomment
  :bind (("C-c c" . comment-region)
         ("C-c u" . uncomment-region)))
