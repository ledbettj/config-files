(require 'use-package)

(use-package browse-at-remote :ensure t :pin melpa
  :init
  (unless (eq system-type 'darwin)
    (setq-default
     browse-url-browser-function 'browse-url-generic
     browse-url-generic-program  "google-chrome-stable"))
  :bind (("C-c h" . browse-at-remote/browse)))
