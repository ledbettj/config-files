;;; ui.el - user interface customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>
(require 'color)

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
        "Ubuntu Mono-13")))

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

;; show tabs and trailing whitespace as slightly darker background color
(add-hook 'font-lock-mode-hook
  (lambda ()
    (font-lock-add-keywords
      'enh-ruby-mode
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
    (set-face-background 'region  (color-lighten-name (face-foreground 'font-lock-string-face) 10))
    (set-face-background 'hl-line (color-lighten-name bg 8.5))
    (set-face-foreground 'linum   (color-lighten-name bg 15))
    (set-face-background 'linum   (color-darken-name bg 5))
    (set-face-background 'trailing-whitespace (color-darken-name bg 25))
    (set-face-foreground 'which-func (face-foreground 'font-lock-keyword-face))
    (set-face-background 'mode-line (color-darken-name bg 15))
    (set-face-foreground 'mode-line (color-darken-name fg 5))
    (set-face-background 'mode-line-inactive (color-darken-name bg 25))
    (set-face-foreground 'mode-line-inactive (color-darken-name fg 35))
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'company-preview-common nil
      :background (color-lighten-name bg 20)
      :foreground fg
      :underline t
      :box nil)
    (set-face-attribute 'company-preview nil
      :background (color-lighten-name bg 20)
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
    (set-face-background 'company-scrollbar-bg (color-darken-name fg 15))
    (set-face-background 'company-scrollbar-fg (color-darken-name fg 35))))

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
          (`not-checked " ?")
          (`no-checker (propertize " -" 'face 'warning))
          (`running (propertize " ❂" 'face 'escape-glyph))
          (`errored (propertize " ⚠" 'face 'error))
          (`finished
            (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                            (no-warnings 'warning)
                            (t 'success))))
              (propertize (if (or no-errors no-warnings) " ✘" " ✔") 'face face)))
          (`interrupted " ⚠")
          (`suspicious '(propertize " ⚠" 'face 'warning)))))

(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) nil)
