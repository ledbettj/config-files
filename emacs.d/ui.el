;;; ui.el - user interface customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

;; disable menu bar, tool bar, and scroll bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))

(load-theme 'wombat t nil)

(setq inhibit-startup-screen t) ; don't show the splash screen

;; set default font to Consolas on OS X, or Ubuntu Monospace otherwise.
(setq default-frame-alist
      `((font . ,(if (eq system-type 'darwin) "Consolas-16" "Ubuntu Mono-12"))))

;; set frame title to user@host: <buffer> [modified?]
(setq frame-title-format
      (list
       (user-login-name)
       "@"
       (system-name)
       ": %b %+" ))


(which-func-mode t) ; show current function in mode line
(show-paren-mode t) ; highlight matching parentheses

(setq line-number-mode   t) ; show line number in mode line
(setq column-number-mode t) ; show column number in mode line
