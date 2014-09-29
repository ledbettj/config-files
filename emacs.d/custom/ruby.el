;;; ruby.el - ruby mode customization for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

(setq auto-mode-alist
  (append auto-mode-alist
    '(("\\.rb$"       . enh-ruby-mode)
       ("Gemfile$"    . enh-ruby-mode)
       ("Rakefile$"   . enh-ruby-mode)
       ("\\.gemspec$" . enh-ruby-mode)
       ("\\.ru"       . enh-ruby-mode)
       ("\\.rake"     . enh-ruby-mode))))


(defun convert-hash-rocket (BEG END)
  "Convert hash rocket syntax to JSON syntax"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward ":\\([^\s]+\\)\s*=>\s*\\([^\s]+\\)" END t)
        (replace-match "\\1: \\2")))))

(defun jl/ruby-setup ()
  (local-set-key (kbd "C-c b") 'magit-blame-mode)
  (local-set-key (kbd "C-c r") 'convert-hash-rocket)
  (rainbow-mode t)
  (set-face-foreground 'enh-ruby-op-face (face-foreground 'default))
  (ruby-electric-mode t))

(add-hook 'enh-ruby-mode-hook 'jl/ruby-setup)

(eval-after-load "enh-ruby-mode"
  '(progn
    (setq enh-ruby-bounce-deep-indent t) ; tab toggles between deep indent
    (setq enh-ruby-check-syntax nil)))   ; flycheck can handle this
