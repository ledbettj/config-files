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
(setq initial-scratch-message
";;   ___ _ __ ___   __ _  ___ ___ _
;;  / _ \\ '_ ` _ \\ / _` |/ __/ __| |
;; |  __/ | | | | | (_| | (__\\__ \\_|
;;  \\___|_| |_| |_|\\__,_|\\___|___(_)
")
(load jl/custom-file t)

(fset 'startup-echo-area-message
  #'(lambda ()
      (message "emacs loaded in %s" (emacs-init-time))))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

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

(require 'diminish)
(require 'bind-key)

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

(use-package load-dir
  :ensure t
  :pin melpa
  :defer 1
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-file jl/prefs)
  (load-dir-one jl/init-dir))
