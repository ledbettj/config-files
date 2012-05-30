;;; init.el - emacs configuration file
;; John Ledbetter <john.ledbetter@gmail.com>
(require 'package)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; install el-get if necessary
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
       (lambda (s)
	 (let (el-get-master-branch)
	   (goto-char (point-max))
	   (eval-print-last-sexp)))))

;; custom packages
(setq el-get-sources
      '((:name scss-mode
	       :type git
	       :url "http://github.com/antonj/scss-mode.git")))

;; list of packages to have el-get install
(defvar required-packages
  '(auto-complete
    auto-complete-css
    auto-complete-emacs-lisp
    flymake-point
    flymake-ruby
    rainbow-mode
    ruby-electric
    scss-mode))

(el-get 'sync required-packages)

;; turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

(defun load-user-file (file)
  (interactive "f")
  "Load a file in the user's current configuration directory."
  (load-file (expand-file-name file user-emacs-directory)))

;; store emacs auto-customization in its own file.
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load user customization
(load-user-file "ui.el")          ; UI tweaks (font, colors, etc.)
(load-user-file "modes.el")
(load-user-file "behavior.el")
(load-user-file "move-text.el")   ; M-Up and M-Down move blocks of text around


;; display startup timing after load
(fset 'startup-echo-area-message
      '(lambda ()
	 (message "emacs loaded in %s" (emacs-init-time))))
