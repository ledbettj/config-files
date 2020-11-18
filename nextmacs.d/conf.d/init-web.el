(use-package web-mode :ensure t :defer t
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  :mode (".html.erb" ".html?" ".hbs"))

(use-package typescript-mode :ensure t :defer t
  :config
  (setq-default typescript-indent-level 2))


(setq-default js-indent-level 2)
