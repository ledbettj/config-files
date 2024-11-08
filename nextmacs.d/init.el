(require 'package)

(defconst jl/custom-file "~/.emacs.d/auto-custom.el")
(defconst jl/init-dir    "~/.emacs.d/conf.d")
(defconst jl/prefs       "~/.emacs.d/prefs.el")

(setq load-prefer-newer         t)
(setq custom-file               jl/custom-file)
(setq package-enable-at-startup nil)
(setq inhibit-splash-screen     t)
(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq initial-scratch-message "")
(setq create-lockfiles nil)

(load jl/custom-file t)

(fset 'startup-echo-area-message
  #'(lambda ()
      (message "emacs loaded in %s" (emacs-init-time))))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(package-initialize)

(eval-when-compile
  (package-initialize)

  (if (null (require 'use-package nil t))
    (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                (progn (package-refresh-contents)
                                       package-archive-contents)
                              package-archive-contents))
                  (AVAIL (assoc 'use-package ARCHIVES)))
             (if AVAIL
                 (package-install 'use-package)))
           (require 'use-package))))

(use-package auto-compile :ensure t
  :config
  (auto-compile-on-save-mode))

;; quick global configuration ...
(setq-default kill-whole-line  1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width        2)
(setq-default confirm-nonexistent-file-or-buffer nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box      nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
;; then let's load everything else.

;; lsp performance configurations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

(use-package load-dir
  :ensure t
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (let ((gc-cons-threshold most-positive-fixnum))
    (load-file jl/prefs)
    (load-dir-one jl/init-dir)))
