(use-package lsp-metals :ensure t
  :config
  (setq lsp-metals-treeview-show-when-views-received nil))

(use-package scala-mode :ensure t
  :config
  :hook (scala-mode . (lambda ()
                       (lsp)
                       (lsp-ui-mode)
                       (lsp-ui-sideline-mode))))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (setq comint-terminfo-terminal "vte-256color")
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
