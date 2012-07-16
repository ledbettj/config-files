;;; modes.el - hooks for customizing various modes
;; John Ledbetter <john.ledbetter@gmail.com>

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (setq lisp-indent-offset 2)
	     (rainbow-mode t)))
