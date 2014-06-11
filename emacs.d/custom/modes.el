;;; modes --- hooks for customizing various modes
;; John Ledbetter <john.ledbetter@gmail.com>

;(add-to-list 'auto-mode-alist '("\\.vala\\'" . vala-mode))

(add-hook 'markdown-mode-hook
  '(lambda ()
     (flyspell-mode t)))

(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (setq lisp-indent-offset 2)
     (rainbow-mode t)))

(add-hook 'scss-mode-hook
  '(lambda ()
     (setq scss-compile-at-save nil)
     (setq css-indent-offset 2)
     (auto-complete-mode t)
     (rainbow-mode t)))

(add-hook 'css-mode-hook
  '(lambda ()
     (setq css-indent-offset 2)
     (rainbow-mode t)))

(add-hook 'rhtml-mode-hook
  '(lambda ()
     (rainbow-mode t)
     (auto-complete-mode t)))

(add-hook 'sh-mode-hook
  '(lambda ()
     (setq sh-basic-offset 2)))

(add-hook 'coffee-mode-hook
  '(lambda ()
     (auto-complete-mode t)
     (make-local-variable 'tab-width)
     (set 'tab-width 2)))

(add-hook 'go-mode-hook
  '(lambda ()
     (auto-complete-mode t)
     (setq indent-tabs-mode nil)))

(add-hook 'vala-mode-hook
  '(lambda ())
     (auto-complete-mode t)
     (setq indent-tabs-mode nil))

(add-hook 'dired-mode-hook 'rspec-dired-mode)
