(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook '(lambda ()
                                     (setq mode-name (all-the-icons-icon-for-file "foo.el"
                                            :height 0.9 :v-adjust -0.2)))))
