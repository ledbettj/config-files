(use-package flycheck :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-checkers (delete 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-indication-mode nil))
