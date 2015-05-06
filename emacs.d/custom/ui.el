;;; ui.el - user interface customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

;; disable menu bar, tool bar, and scroll bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'fringe-mode)     (fringe-mode 0))
(unless (eq system-type 'darwin)
  (if (fboundp 'menu-bar-mode)   (menu-bar-mode -1)))

(which-function-mode -1)        ; show current function in mode line
(show-paren-mode t)             ; highlight matching parentheses
(setq line-number-mode   t)     ; show line number in mode line
(setq column-number-mode t)     ; show column number in mode line
(setq inhibit-startup-screen t) ; don't show splash screen
(setq initial-scratch-message nil) ; don't show scratch placeholder

(add-to-list 'default-frame-alist
  `(font .
     ,(if (eq system-type 'darwin)
        (if (eq (display-pixel-width) 1280)
          "Ubuntu Mono-16"
          "Ubuntu Mono-18")
        "Ubuntu Mono-12")))

(set-face-attribute 'default nil :weight 'light)

(add-to-list 'default-frame-alist '(width . 84))

;; set frame title to user@host: <buffer> [modified?]
(setq frame-title-format
  (list
    (user-login-name)
    "@"
    (system-name)
    ": %b %+" ))

;; I love rainbow mode so much
(eval-after-load "rainbow-mode"
  '(progn
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
        (min (* (/ r 65280.0) 256) 255)
        (min (* (/ g 65280.0) 256) 255)
        (min (* (/ b 65280.0) 256) 255)))
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

(defadvice server-create-window-system-frame (after do-some-scaling ())
  (scale-ui-colors))
(defadvice enable-theme (after do-some-scaling ())
  (scale-ui-colors))

(ad-activate 'server-create-window-system-frame)
(ad-activate 'enable-theme)

(defun scale-ui-colors ()
  (let ((bg (face-background 'default))
         (fg (face-foreground 'default)))
    (set-face-background 'region  (scale-colour (face-foreground 'font-lock-string-face) 1.55))
    (set-face-background 'hl-line (scale-colour bg 1.20))
    (set-face-foreground 'linum   (scale-colour bg 1.50))
    (set-face-background 'linum   (scale-colour bg 0.90))
    (set-face-background 'trailing-whitespace (scale-colour bg 0.83))
    (set-face-foreground 'which-func (face-foreground 'font-lock-keyword-face))
    (set-face-background 'mode-line (scale-colour bg 0.75))
    (set-face-foreground 'mode-line (scale-colour fg 0.75))
    (set-face-background 'mode-line-inactive (scale-colour bg 0.65))
    (set-face-foreground 'mode-line-inactive (scale-colour fg 0.65))
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line nil :box (scale-colour fg 0.50))
    (set-face-attribute 'company-preview-common nil
      :background (scale-colour bg 1.20)
      :foreground fg
      :underline t
      :box nil)
    (set-face-attribute 'company-preview nil
      :background (scale-colour bg 1.20)
      :foreground fg
      :underline t
      :box nil)
    (set-face-attribute 'company-tooltip nil
      :background fg
      :foreground bg)
    (set-face-attribute 'company-tooltip-common-selection nil
      :background nil
      :foreground nil
      :bold t
      )
    (set-face-attribute 'company-tooltip-selection nil
      :background (face-foreground 'font-lock-keyword-face)
      :foreground "#000000")
    (set-face-attribute 'company-tooltip-common nil
      :background nil
      :foreground nil
      :underline nil
      :bold t)
    (set-face-background 'company-scrollbar-bg (scale-colour fg 0.85))
    (set-face-background 'company-scrollbar-fg (scale-colour fg 0.65))))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green3")
     (set-face-foreground 'diff-removed "red4")))

(eval-after-load "auto-complete" '(progn (scale-ui-colors)))


;; monokai has a green background in a terminal. don't use it.
(load-theme (if (window-system) 'monokai 'wombat) t nil)

(unless (eq system-type 'darwin)
  (set-fontset-font t '(#x1f300 . #x1f5ff) "Symbola")
  (set-fontset-font t '(#x1f600 . #x1f64f) "Symbola")
  (set-fontset-font t '(#x1f680 . #x1f6ff) "Symbola")
  (set-fontset-font t '(#x2600  . #x26ff)  "Symbola")
  (set-fontset-font t '(#x4e00  . #x9fff)  "Noto"))

(when (eq system-type 'darwin)
  (setq ns-use-srgb-colorspace t)
  (setq ns-use-native-fullscreen t))

(set-face-attribute 'mode-line nil :height 1.0)

(setq flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          (`not-checked " Fly")
          (`no-checker (propertize " NoFly" 'face 'warning))
          (`running (propertize " Fly" 'face 'success))
          (`errored (propertize " Fly" 'face 'error))
          (`finished
            (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                            (no-warnings 'warning)
                            (t 'success))))
              (propertize " Fly" 'face face)))
          (`interrupted " Fly~")
          (`suspicious '(propertize "Fly" 'face 'warning)))))

(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) nil)

(eval-after-load "rspec-mode"
  '(progn
     (setcar (cdr (assq 'rspec-mode minor-mode-alist)) " RS")))


(let ((mode-line-font (if (eq system-type 'darwin)
                        "Ubuntu Condensed-16"
                        "Ubuntu Condensed-12")))
  
  (set-face-font 'mode-line mode-line-font)
  (set-face-font 'mode-line-inactive mode-line-font))
