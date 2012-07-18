;;; init.el - emacs configuration file
;; John Ledbetter <john.ledbetter@gmail.com>
(require 'package)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; install el-get if necessary
(unless (require 'el-get nil t)
  (let ((buf
          (url-retrieve-synchronously
            "https://raw.github.com/dimitri/el-get/master/el-get-install.el")))
    (if buf
      (with-current-buffer buf
        (end-of-buffer)
        (eval-print-last-sexp)))))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; custom packages
(setq el-get-sources
  '((:name scss-mode
      :type git
      :url "http://github.com/antonj/scss-mode.git")
     (:name rainbow-mode
       :type elpa)
     (:name ruby-electric
       :type git
       :url "http://github.com/leoc/ruby-electric.git")))

;; list of packages to have el-get install
(defvar required-packages
  '(auto-complete
     auto-complete-css
     auto-complete-emacs-lisp
     flymake-point
     flymake-ruby
     markdown-mode
     rainbow-mode
     ruby-electric
     scss-mode
     yaml-mode))

(el-get 'sync required-packages)

(push (expand-file-name "themes" user-emacs-directory) custom-theme-load-path)

;; turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

(defun load-user-file (file)
  (interactive "f")
  "Load a file in the user's current configuration directory."
  (load-file (expand-file-name file
               (expand-file-name "customization" user-emacs-directory))))

;; store emacs auto-customization in its own file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load user customization
(load-user-file "ui.el")          ; UI tweaks (font, colors, etc.)
(load-user-file "modes.el")
(load-user-file "ruby.el")
(load-user-file "js.el")
(load-user-file "behavior.el")
(load-user-file "move-text.el")
(load-user-file "keybinds.el")

;; display startup timing after load
(fset 'startup-echo-area-message
  '(lambda ()
     (message "emacs loaded in %s" (emacs-init-time))))
