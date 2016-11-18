(use-package elixir-mode :ensure t
  :init
  (add-hook 'elixir-mode-hook '(lambda () (setq mode-name (all-the-icons-icon-for-file "foo.ex" :v-adjust 0.045 :height 0.9)))))
(use-package alchemist :ensure t)
