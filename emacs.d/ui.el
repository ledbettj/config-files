;;; ui.el - user interface customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

;; disable menu bar, tool bar, and scroll bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))

(which-func-mode t)             ; show current function in mode line
(show-paren-mode t)             ; highlight matching parentheses
(setq line-number-mode   t)     ; show line number in mode line
(setq column-number-mode t)     ; show column number in mode line
(setq inhibit-startup-screen t) ; don't show splash screen

(load-theme 'wombat t nil)      ; currently selected theme


;; set default font to Consolas on OS X, or Ubuntu Monospace otherwise.
(setq default-frame-alist
      `((font . ,(if (eq system-type 'darwin) "Consolas-16" "Ubuntu Mono-14"))))

;; set frame title to user@host: <buffer> [modified?]
(setq frame-title-format
      (list
       (user-login-name)
       "@"
       (system-name)
       ": %b %+" ))

;; customize autocomplete colors
(eval-after-load "auto-complete"
  '(lambda ()
     (set-face-attribute 'ac-candidate-face nil
			 :background (face-foreground 'default)
			 :foreground (face-background 'default)
			 :box nil)
     (set-face-attribute 'ac-selection-face nil
			 :background (face-foreground 'font-lock-keyword-face)
			 :foreground "#000000"
			 :bold t
			 :box nil)))

;; I love rainbow mode so much
(eval-after-load "rainbow-mode"
  '(lambda ()
     (nconc rainbow-html-colors-major-mode-list
	    '(scss-mode emacs-lisp-mode javascript-mode))))
