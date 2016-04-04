(require 'use-package)

(defun font-exists-p (font)
  "check if the specified font is present on the system"
  (if (null (x-list-fonts font)) nil t))

;; preferred fonts, in order. the first one found on the system will be used.
(defconst jl/preferred-fonts '(
  "Hack"
  "Source Code Pro"
  "Ubuntu Mono"
  "Monaco"))

(defconst jl/use-font (cl-find-if 'font-exists-p jl/preferred-fonts))

(when (eq system-type 'darwin)
  (setq ns-use-srgb-colorspace   t)
  (setq ns-use-native-fullscreen t))

;; turn off scrollbar
(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; turn off tool bar
(use-package tool-bar
  :config
  (tool-bar-mode -1))

;; turn off menu bar
(use-package menu-bar
  :config
  (menu-bar-mode -1))

;; set up fringe only on the right side, for the yascroll
(use-package fringe
  :config
  (set-fringe-mode '(0 . 8)))

(use-package solarized-theme :ensure t :pin melpa
  :init
  (load-theme 'solarized-dark t)
  :config
  ;; don't use variable height text in org mode.
  (setq-default solarized-use-variable-pitch nil)
  (setq-default solarized-scale-org-headlines nil))

;; fancy mac-style scroll bar.
(use-package yascroll :ensure t :pin melpa
  :config
  (global-yascroll-bar-mode 1))

(use-package simple
  :init
  (which-function-mode -1) ; turn off function display in modelinae
  (show-paren-mode      1) ; highlight matching parens
  (line-number-mode     1) ; show line number in mode line
  (column-number-mode   1) ; show column number in mode line
  (setq-default show-trailing-whitespace 1) ; show trailing whitespace.
  (setq frame-title-format ; user@host *modified
	(list
	 (user-login-name)
	 "@"
	 (system-name)
	 ": %b %+"))

  ;; use 12px on Linux, 16px on Mac built in screen, 18x on Mac large screen.
  (add-to-list 'default-frame-alist
               `(font .
                      ,(if (eq system-type 'darwin)
                           (if (eq (display-pixel-width) 1280)
                               (concat jl/use-font "-16")
                             (concat jl/use-font "-18"))
                               (concat jl/use-font "-12"))))
  ;; default window width is 84 columns.
  (add-to-list 'default-frame-alist '(width . 84)))

(use-package linum
  :config
  (global-linum-mode 1)
  (setq-default linum-format "%02d "))

(use-package hl-line
  :config
  (global-hl-line-mode 1) ; always highlight current line
  (setq-default hl-line-sticky-flag nil)) ; except in an inactive buffer

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
			  :underline  nil)

      (set-face-attribute 'flycheck-error nil
                          :underline '(:color "Red1" :style line))
      (set-face-attribute 'flycheck-info nil
                          :underline `(:color ,com :style line))
      (set-face-attribute 'flycheck-warning nil
                          :underline '(:color "DarkOrange" :style line))

    (set-face-attribute 'company-preview-common nil
                        :background (color-lighten-name bg 20)
                        :foreground fg
                        :underline  t
                        :box        nil)

    (set-face-attribute 'company-preview nil
                        :background (color-lighten-name bg 20)
                        :foreground fg
                        :underline  t
                        :box        nil)

    (set-face-attribute 'company-tooltip nil
                        :background fg
                        :foreground bg)

    (set-face-attribute 'company-tooltip-common-selection nil
                        :background nil
                        :foreground nil
                        :bold       t)

    (set-face-attribute 'company-tooltip-selection nil
                        :background key
                        :foreground "#000000")

    (set-face-attribute 'company-tooltip-common nil
                        :background nil
                        :foreground nil
                        :underline  nil
                        :bold       t)

    (set-face-background 'company-scrollbar-bg (color-darken-name fg 15))
    (set-face-background 'company-scrollbar-fg (color-darken-name fg 35))))
  (scale-ui-colors))

(use-package abbrev
  :config
  (setcar (cdr (assq 'abbrev-mode minor-mode-alist)) nil))
