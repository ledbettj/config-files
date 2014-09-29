;;; ruby.el - ruby mode customization for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

;; fix ruby-mode multi-line parameter indentation
(setq-default ruby-deep-indent-paren       nil)
(setq-default ruby-deep-indent-paren-style nil)
(setq-default ruby-deep-arglist            nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
         indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; add additional filenames to ruby-mode
(setq auto-mode-alist
  (append auto-mode-alist
    '(("Gemfile$"    . ruby-mode)
       ("Rakefile$"   . ruby-mode)
       ("\\.gemspec$" . ruby-mode)
       ("\\.ru"       . ruby-mode)
       ("\\.rake"     . ruby-mode))))


(defun convert-hash-rocket (BEG END)
  "Convert hash rocket syntax to JSON syntax"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward ":\\([^\s]+\\)\s*=>\s*\\([^\s]+\\)" END t)
        (replace-match "\\1: \\2")))))


(add-hook 'ruby-mode-hook
  '(lambda ()
     (local-set-key (kbd "C-c b") 'magit-blame-mode)
     (local-set-key (kbd "C-c r") 'convert-hash-rocket)
     (rainbow-mode t)
     (ruby-electric-mode t)))
