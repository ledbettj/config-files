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

(use-package enh-ruby-mode :ensure t :pin melpa
  :bind (("C-c r" . convert-hash-rocket))
  :diminish "Ruby+"
  :init
  (setq auto-mode-alist
        (append
         '(("\\.rb$"       . enh-ruby-mode)
           ("Gemfile$"    . enh-ruby-mode)
           ("Rakefile$"   . enh-ruby-mode)
           ("\\.gemspec$" . enh-ruby-mode)
           ("\\.cap$"     . enh-ruby-mode)
           ("\\.thor$"    . enh-ruby-mode)
           ("\\.rake$"    . enh-ruby-mode))
         auto-mode-alist))
  :config
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  (setq-default enh-ruby-extra-keywords (list "public" "private" "protected"))
  (erm-reset)
  (setq enh-ruby-bounce-deep-indent t) ; tab toggles between deep indent
  (setq enh-ruby-check-syntax nil))   ; flycheck can handle this
