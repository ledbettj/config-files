;;; ui.el - user interface customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

;; disable menu bar, tool bar, and scroll bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(unless (eq system-type 'darwin)
  (if (fboundp 'menu-bar-mode)   (menu-bar-mode -1)))

(which-function-mode t)         ; show current function in mode line
(show-paren-mode t)             ; highlight matching parentheses
(setq line-number-mode   t)     ; show line number in mode line
(setq column-number-mode t)     ; show column number in mode line
(setq inhibit-startup-screen t) ; don't show splash screen
(setq initial-scratch-message nil) ; don't show scratch placeholder
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; set default font to Consolas on OS X, or Ubuntu Monospace otherwise.
(add-to-list 'default-frame-alist
  `(font .
     ,(if (eq system-type 'darwin)
        "Source Code Pro-16"
        "Bitstream Vera Sans Mono-12")))

(set-face-attribute 'default nil :weight 'light)

(add-to-list 'default-frame-alist '(width . 84))

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

(eval-after-load "flycheck-mode"
  '(lambda ()
     (set-face-attribute 'flycheck-error nil
       :underline '(:style line :color "Red1"))
     (set-face-attribute 'flycheck-warning nil
       :underline '(:style line :color "DarkOrange"))
     (set-face-attribute 'flycheck-info nil
       :underline '(:style line :color "ForestGreen"))))


;; I love rainbow mode so much
(eval-after-load "rainbow-mode"
  '(lambda ()
     (nconc rainbow-html-colors-major-mode-list
       '(scss-mode emacs-lisp-mode javascript-mode))))

(defun scale-colour (colour factor)
  "Scale the given hex colour (#112233) by the given factor.
This used specifically to make whitespace appear as a slightly darker color
than the background of the buffer."
  (if window-system
    (let* ((values (color-values colour))
            (r (floor (* factor (car values))))
            (g (floor (* factor (cadr values))))
            (b (floor (* factor (caddr values)))))
      (format "#%02x%02x%02x"
        (* (/ r 65280.0) 256)
        (* (/ g 65280.0) 256)
        (* (/ b 65280.0) 256)))
    colour))


;; show tabs and trailing whitespace as slightly darker background color
(add-hook 'font-lock-mode-hook
  (lambda ()
    (font-lock-add-keywords
      nil
      '(("\t" 0 'trailing-whitespace prepend)))))

(setq-default show-trailing-whitespace t)

(global-linum-mode t)
(global-hl-line-mode t)
(setq-default linum-format "%02d ")
(setq-default hl-line-sticky-flag nil)

(set-face-attribute 'mode-line          nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(defadvice server-create-window-system-frame (after do-some-scaling ())
  (scale-ui-colors))
(defadvice enable-theme (after do-some-scaling ())
  (scale-ui-colors))

(ad-activate 'server-create-window-system-frame)
(ad-activate 'enable-theme)

(defun scale-ui-colors ()
  (let ((bg (face-background 'default))
        (fg (face-foreground 'default)))
    (set-face-background 'hl-line (scale-colour bg 1.20))
    (set-face-foreground 'linum   (scale-colour bg 1.50))
    (set-face-background 'linum   (scale-colour bg 0.90))
    (set-face-background 'trailing-whitespace (scale-colour bg 0.83))
    (set-face-foreground 'which-func (face-foreground 'font-lock-keyword-face))
    (set-face-background 'mode-line (scale-colour bg 0.75))
    (set-face-foreground 'mode-line (scale-colour fg 0.75))
    (set-face-background 'mode-line-inactive (scale-colour bg 0.65))
    (set-face-foreground 'mode-line-inactive (scale-colour fg 0.65))))


(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green3")
     (set-face-foreground 'diff-removed "red4")))

(load-theme 'monokai t nil)
