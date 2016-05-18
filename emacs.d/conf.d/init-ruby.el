(require 'use-package)

(defun convert-hash-rocket (BEG END)
  "Convert hash rocket syntax to JSON syntax"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward ":\\([^\s]+\\)\s*=>\s*\\([^\s]+\\)" END t)
        (replace-match "\\1: \\2")))))

(use-package robe :ensure t :pin melpa
  :diminish robe-mode
  :init
  (add-hook 'ruby-mode-hook 'robe-mode))
;; this is super slow and not very helpful in rails.
  ;; :config
  ;; (eval-after-load 'company
;;   '(push 'company-robe company-backends)))

(use-package :ruby-mode
  :bind (
         :map ruby-mode-map
              ("C-c r" . convert-hash-rocket)))

(use-package rspec-mode :ensure t :pin melpa
  :diminish (rspec-mode . " Ⓡ")
  :init
  (add-hook 'ruby-mode-hook #'rspec-mode))
