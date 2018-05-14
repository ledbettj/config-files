
(use-package dashboard :ensure t :pin melpa
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5) (projects . 5)))
  :config
  (dashboard-setup-startup-hook))
