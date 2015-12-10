;;; init.el - emacs configuration file
;; John Ledbetter <john.ledbetter@gmail.com>
(setq gc-cons-threshold (* 8192 8192))

(require 'package)

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
  '( (:name rainbow-mode  :type elpa)
     (:name flycheck      :type elpa)
     (:name go-mode       :type elpa)
     (:name enh-ruby-mode :type elpa)
     (:name rinari        :type elpa)
     (:name move-text     :type elpa)))

;; list of packages to have el-get install
(defvar required-packages
  '(buffer-move
     coffee-mode
     company-mode
     company-tern
     enh-ruby-mode
     delight
     fic-mode
     flycheck
     git-timemachine
     go-mode
     haml-mode
     helm
     ido-vertical-mode
     iedit
     jinja2-mode
     inf-ruby
     lua-mode
     magit
     markdown-mode
     move-text
     projectile
     rainbow-mode
     rspec-mode
     rinari
     rust-mode
     scss-mode
     smartparens
     solarized-emacs
     tern
     toml-mode
     tomorrow-theme
     vala-mode
     web-mode
     yaml-mode))

(unless (eq system-type 'darwin)
  (nconc required-packages '(svg-mode-line-themes)))

;; override notifications to be displayed in the message buffer if
;; we're running in a terminal.
(unless (display-graphic-p)
  (defun el-get-notify (title msg)
    (message "%s: %s" title msg)))

(el-get 'sync required-packages)
(package-initialize)

(push (expand-file-name "themes" user-emacs-directory) custom-theme-load-path)

;; turn on autocomplete
(require 'projectile)
(require 'helm-projectile)
(require 'company)
(require 'company-dabbrev-code)
(setq company-dabbrev-code-modes
  (add-to-list 'company-dabbrev-code-modes 'enh-ruby-mode))

(projectile-global-mode)

(defun load-user-file (file)
  "Load the file FILE in the user's current configuration directory."
  (interactive "f")
  (let* ((user-dir (expand-file-name "custom" user-emacs-directory))
          (custom-file (expand-file-name file user-dir)))
    (if (file-exists-p custom-file)
      (load custom-file nil t)
      (message "custom file %s not found" file)
    )))

;; store emacs auto-customization in its own file.
(setq custom-file (expand-file-name "auto-custom.el" user-emacs-directory))
(load custom-file 'noerror)

(map nil 'load-user-file
  '( "paths.el"
     "ui.el"
     "modeline.el"
     "behavior.el"
     "modes.el"
     "ruby.el"
     "js.el"
     "c.el"
     "org.el"
     "magic-align.el"
     "keybinds.el"))

(setq magit-last-seen-setup-instructions "1.4.0")

;; display startup timing after load
(fset 'startup-echo-area-message
  #'(lambda ()
      (message "emacs loaded in %s" (emacs-init-time))))
