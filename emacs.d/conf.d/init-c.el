(use-package cc-mode
  :init
  (add-hook 'c-mode-hook '(lambda () (setq mode-name (all-the-icons-icon-for-file "foo.c" :v-adjust 0.045 :height 0.9))))
  :config
  (setq-default c-default-style "linux")
  (setq-default c-basic-offset 4))
