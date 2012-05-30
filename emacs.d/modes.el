;;; modes.el - hooks for customizing various modes
;; John Ledbetter <john.ledbetter@gmail.com>

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (rainbow-mode t)))
