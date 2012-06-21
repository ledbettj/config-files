;;; modes.el - hooks for customizing various modes
;; John Ledbetter <john.ledbetter@gmail.com>

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (rainbow-mode t)))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (rainbow-mode t)
	     (ruby-electric-mode t)
	     (electric-pair-mode t)))
