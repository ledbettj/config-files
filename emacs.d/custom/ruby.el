;;; ruby.el - ruby mode customization for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

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

(setq interpreter-mode-alist
  (append
    '(("ruby1.8"  . enh-ruby-mode)
       ("ruby1.9" . enh-ruby-mode)
       ("jruby"   . enh-ruby-mode)
       ("rbx"     . enh-ruby-mode)
       ("ruby"    . enh-ruby-mode))
    interpreter-mode-alist))

(defun convert-hash-rocket (BEG END)
  "Convert hash rocket syntax to JSON syntax"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward ":\\([^\s]+\\)\s*=>\s*\\([^\s]+\\)" END t)
        (replace-match "\\1: \\2")))))

(defun symbolify-hash (BEG END)
  "Convert a string hash into a symbol hash"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward "['\"]\\(.+\\)['\"]\s+=>\s+\\(.+\\),?$" END t)
        (replace-match "\\1: \\2")))))

(defun jl/ruby-setup ()
  (local-set-key (kbd "C-c b") 'magit-blame)
  (local-set-key (kbd "C-c r") 'convert-hash-rocket)
  (local-set-key (kbd "C-c q") 'toggle-quotes)
  (rainbow-mode t)
  (smartparens-mode t))

(add-hook 'enh-ruby-mode-hook 'jl/ruby-setup)

(eval-after-load "enh-ruby-mode"
  '(progn
     (setq-default enh-ruby-extra-keywords (list "public" "private" "protected"))
     (erm-reset)
     (setq enh-ruby-bounce-deep-indent t) ; tab toggles between deep indent
     (setq enh-ruby-check-syntax nil)))   ; flycheck can handle this

(eval-after-load "smartparens"
  '(progn
     (require 'smartparens-ruby)
     (setq-default sp-autoescape-string-quote nil)
     (setq-default sp-highlight-pair-overlay  nil)))
