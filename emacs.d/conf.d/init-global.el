(require 'use-package)

(global-set-key (kbd "M-g") 'goto-line)

(unless (eq system-type 'darwin)
  (setq-default
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program  "google-chrome-stable"))

(when (eq system-type 'darwin)
  (setq-default ns-function-modifier 'control) ; laptop keyboard has Fn where Control should be.
  (unbind-key (kbd "s-t"))) ; don't pop up font dialog.

(use-package hungry-delete
  :load-path "local/"
  :bind (([remap backward-delete-char-untabify] . hungry-delete-backwards)
         ([remap delete-backward-char] . hungry-delete-backwards)
         ([remap c-electric-backspace] . hungry-delete-backwards)))

(use-package sudo
  :load-path "local/"
  :bind (("C-c C-s" . reopen-file-with-sudo)))

(use-package magic-align
  :load-path "local/"
  :bind (("C-c a" . magic-align)))

(use-package move-dup :ensure t :pin melpa
  :bind (([M-up] . md/move-lines-up)
         ([M-down] . md/move-lines-down)))

(use-package newcomment
  :bind (("C-c c" . comment-region)
         ("C-c u" . uncomment-region)))

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
