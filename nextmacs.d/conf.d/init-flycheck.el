(use-package flycheck :ensure t :defer t
  :config
  (setq flycheck-checkers (delete 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-indication-mode nil)
  (set-face-attribute 'flycheck-error nil
                      :underline '(:color "Red1" :style line))
  (set-face-attribute 'flycheck-info nil
                      :underline `(:color ,(face-foreground 'font-lock-comment-face) :style line))
  (set-face-attribute 'flycheck-warning nil
                      :underline '(:color "DarkOrange" :style line))
  :init (global-flycheck-mode))
