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

(defun convert-quotes ()
  "toggle single and double quotes around mark"
  (interactive)
  (save-excursion
    (let (p1 s1 p2 s2 content)
      (skip-chars-backward "^\"'\n")
      (setq p1 (point))
      (setq s1 (string (char-before p1)))
      (when (member s1 '("\"" "'"))
        (skip-chars-forward (concat "^" s1 "\n"))
        (setq p2 (point))
        (when (char-after p2)
          (setq s2 (string (char-after p2)))
          (when (string= s1 s2)
            (setq content (buffer-substring-no-properties p1 p2))
            (cond ((and (string= s1 "\"") (eq (string-match "'" content) nil))
                    (goto-char (- p1 1))
                    (delete-char 1)
                    (insert "'")
                    (goto-char p2)
                    (delete-char 1)
                    (insert "'"))
              ((and (string= s1 "'") (eq (string-match "\"" content) nil))
                    (goto-char (- p1 1))
                    (delete-char 1)
                    (insert "\"")
                    (goto-char p2)
                    (delete-char 1)
                    (insert "\""))
                )))))))

(defun convert-quotes-to-single (BEG END)
  "Convert double quotes to single in region"
  (interactive "r")
  (if (not (region-active-p))
    (message "mark not active")
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward "\\(\"\\)" END t)
        (replace-match "'")))))

(defun jl/ruby-setup ()
  (local-set-key (kbd "C-c b") 'magit-blame)
  (local-set-key (kbd "C-c r") 'convert-hash-rocket)
  (local-set-key (kbd "C-c q") 'convert-quotes)
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
