;;; init.el - emacs configuration file
;; John Ledbetter <john.ledbetter@gmail.com>

(require 'package)

(package-initialize)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; install el-get if necessary
(unless (require 'el-get nil t)
  (let ((buf
          (url-retrieve-synchronously
            "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")))
    (if buf
      (with-current-buffer buf
        (let (el-get-master-branch)
          (goto-char (point-max))
          (eval-print-last-sexp))))))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)

;; custom packages
(setq el-get-sources
  '((:name rainbow-mode
      :type elpa)
     (:name flycheck
       :type elpa)
     (:name go-mode
       :type elpa)
     (:name enh-ruby-mode
       :type elpa)
     (:name move-text
       :type elpa)))

;; list of packages to have el-get install
(defvar required-packages
  '(auto-complete
     auto-complete-css
     auto-complete-emacs-lisp
     coffee-mode
     enh-ruby-mode
     flycheck
     git-timemachine
     go-mode
     haml-mode
     helm
     iedit
     lua-mode
     magit
     markdown-mode
     move-text
     projectile
     rainbow-mode
     rhtml-mode
     rspec-mode
     rust-mode
     scss-mode
     smartparens
     tern
     vala-mode
     yaml-mode))

;; override notifications to be displayed in the message buffer if
;; we're running in a terminal.
(unless (display-graphic-p)
  (defun el-get-notify (title msg)
           (message "%s: %s" title msg)))

(el-get 'sync required-packages)

(push (expand-file-name "themes" user-emacs-directory) custom-theme-load-path)

;; turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

(defun load-user-file (file)
  (interactive "f")
  "Load a file in the user's current configuration directory."
  (load-file (expand-file-name file
               (expand-file-name "custom" user-emacs-directory))))

;; store emacs auto-customization in its own file.
(setq custom-file (expand-file-name "auto-custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load user customization
(load-user-file "paths.el")
(load-user-file "ui.el")
(load-user-file "behavior.el")
(load-user-file "modes.el")
(load-user-file "ruby.el")
(load-user-file "js.el")
(load-user-file "c.el")
(load-user-file "org.el")
(load-user-file "magic-align.el")
(load-user-file "keybinds.el")

;; display startup timing after load
(fset 'startup-echo-area-message
  #'(lambda ()
     (message "emacs loaded in %s" (emacs-init-time))))
