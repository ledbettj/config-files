(require 'use-package)
(require 'all-the-icons)

(defun convert-hash-rocket (BEG END)
  "Convert hash rocket syntax to JSON syntax"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward ":\\([^\s]+\\)\s*=>\s*\\([^\s]+\\)" END t)
        (replace-match "\\1: \\2")))))

(use-package :ruby-mode
  :init
  (add-hook 'ruby-mode-hook '(lambda () (setq mode-name (all-the-icons-icon-for-file "foo.rb"))))
  :bind (
         :map ruby-mode-map
              ("C-c r" . convert-hash-rocket)))

(use-package rspec-mode :ensure t :pin melpa
  :diminish (rspec-mode . " Ⓡ")
  :init
  (add-hook 'ruby-mode-hook #'rspec-mode))
