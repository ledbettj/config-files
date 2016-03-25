(require 'use-package)

(when (eq system-type 'darwin)
  (setq ns-use-srgb-colorspace   t)
  (setq ns-use-native-fullscreen t))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package fringe
  :config
  (set-fringe-mode '(0 . 8)))

(use-package solarized-theme :ensure t :pin melpa
  :init
  (load-theme 'solarized-dark t)
  :config
  (setq-default solarized-use-variable-pitch nil)
  (setq-default solarized-scale-org-headlines nil))

(use-package yascroll :ensure t :pin melpa
  :config
  (global-yascroll-bar-mode 1))

(use-package simple
  :init
  (which-function-mode -1)
  (show-paren-mode      1)
  (line-number-mode     1)
  (column-number-mode   1)
  (setq-default show-trailing-whitespace 1)
  (setq frame-title-format
	(list
	 (user-login-name)
	 "@"
	 (system-name)
	 ": %b %+"))

  (add-to-list 'default-frame-alist
	       `(font .
		      ,(if (eq system-type 'darwin)
			   (if (eq (display-pixel-width) 1280)
			       "Hack-16"
			     "Hack-18")
			 "Hack-12")))
  (add-to-list 'default-frame-alist '(width . 84)))

(use-package linum
  :config
  (global-linum-mode 1)
  (setq-default linum-format "%02d "))

(use-package hl-line
  :config
  (global-hl-line-mode 1)
  (setq-default hl-line-sticky-flag nil))

(use-package rainbow-mode :ensure t :pin melpa
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package fic-mode :ensure t :pin melpa
  :init
  (add-hook 'prog-mode-hook #'fic-mode))

(use-package delight :ensure t :pin melpa
  :config
  (delight '((emacs-lisp-mode "Elisp" :major)
             (enh-ruby-mode "Ruby+"   :major)
             (rspec-mode  " RS")
             (smartparens-mode " ()" smartparens)
             (rainbow-mode " Rbow"))))

(use-package color
  :config
  (defun scale-ui-colors ()
    (let ((bg  (face-background 'default))
	  (fg  (face-foreground 'default))
	  (str (face-foreground 'font-lock-string-face))
	  (com (face-foreground 'font-lock-comment-face))
	  (key (face-foreground 'font-lock-keyword-face)))

      (set-face-background 'trailing-whitespace  (color-darken-name  bg  5))
      (set-face-background 'region               (color-lighten-name str 1))
      (set-face-background 'hl-line              (color-lighten-name bg  8.5))
      (set-face-foreground 'which-func           key)

      (set-face-attribute 'linum nil
			  :background (color-darken-name  bg 3)
			  :foreground (color-lighten-name fg 2))

      (set-face-attribute 'mode-line nil
			  :height     1.0
			  :background (color-darken-name bg 10)
			  :foreground (color-darken-name fg 2)
			  :box        nil
			  :underline  nil)

      (set-face-attribute 'mode-line-inactive nil
			  :height     1.0
			  :background (color-darken-name bg 25)
			  :foreground (color-darken-name fg 35)
			  :box        nil
			  :underline  nil)))
  (scale-ui-colors))
