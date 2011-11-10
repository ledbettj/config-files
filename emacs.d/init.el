;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nconc load-path (list "~/.emacs.d/site-lisp"
                       "~/.emacs.d/el-get/el-get"))

(require 'package)
;(require 'el-get)

(unless (require 'el-get nil t)
  (with-current-buffer
    (url-retrieve-synchronously
      "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(nconc package-archives (list '("tromey" . "http://tromey.com/elpa/")))
(package-initialize)

(setq el-get-sources
  '((:name ruby-mode
      :type elpa
      :load "ruby-mode.el")
     (:name inf-ruby
       :type elpa)
     (:name css-mode
       :type elpa)
     (:name rvm
       :type git
       :url "http://github.com/senny/rvm.el.git"
       :load "rvm.el"
       :compile ("rvm.el")
       :after (lambda() (rvm-use-default)))
     (:name yaml-mode
       :type git
       :url "http://github.com/yoshiki/yaml-mode.git"
       :features yaml-mode)
     (:name rhtml
       :type git
       :url "http://github.com/eschulte/rhtml.git"
       :features rhtml-mode)
     (:name yasnippet
       :type git
       :url "http://github.com/capitaomorte/yasnippet.git"
       :features yasnippet
       :after (lambda()
		(yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")
		(yas/initialize)))
     )
)

(defvar required-packages
  '(ruby-mode inf-ruby css-mode rvm yaml-mode rhtml haml-mode yasnippet
              auto-complete-yasnippet  auto-complete-css
              auto-complete-emacs-lisp auto-complete js2-mode json lua-mode
              markdown-mode coffee-mode flymake-ruby))

(el-get 'sync required-packages)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)
(require 'yasnippet)

(yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")
(yas/initialize)
(ac-config-default)

(setq auto-mode-alist
  (append
    (list
      '("Gemfile$" . ruby-mode)
      '("Rakefile$" . ruby-mode)
      '("\\.gemspec$" . ruby-mode)
      '("\\.ru$" . ruby-mode)
      '("\\.erb$" . rhtml-mode)
      '("\\.yaml$" . yaml-mode)
      '("\\.yml$" . yaml-mode)
      )
    auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq line-number- t)
(setq column-number-mode t)
(setq frame-title-format (list (user-login-name) "@" (system-name) ": %b %+" ))
(show-paren-mode t)
(set-frame-font "Bitstream Vera Sans Mono-10")
(setq inhibit-startup-message t)
(which-function-mode t)
(global-font-lock-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'wombat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default kill-whole-line t)
(setq-default require-final-newline nil)
(setq-default next-line-add-newlines nil)
(setq-default show-trailing-whitespace t)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(delete-selection-mode t)
(setq-default fill-column 80)
(iswitchb-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'on-text-mode)
(add-hook 'c-mode-hook 'on-c-like-mode)

(defun on-text-mode ()
  (flyspell-mode t))

(defun on-c-like-mode ()
  (flyspell-prog-mode))
