(defvar jl/theme-initialized nil)

(use-package doom-themes :ensure t)

(setq-default show-trailing-whitespace 1)



;; setup a frame with the appropriate UI settings.
;; this is either initial-frame-alist to apply to the already created initial frame,
;; or default-frame-alist to setup new frames.
(defun jl/set-frame-params (frame-alist)
  (add-to-list frame-alist
               `(font .
                      ,(concat prefs/use-font "-"
                               (number-to-string (prefs/use-font-size)))))
  ;; default window width is 84 columns.
  (add-to-list frame-alist '(width . 84))
  ;; no scrolly
  (add-to-list frame-alist '(vertical-scroll-bars . nil))
  (add-to-list frame-alist '(horizontal-scroll-bars . nil))
  ;; no menu bar or toolbar
  (add-to-list frame-alist '(menu-bar-lines . 0))
  (add-to-list frame-alist '(tool-bar-lines . 0))

  (unless jl/theme-initialized
    (setq jl/theme-initialized t)
    (load-theme prefs/use-theme))
  (set-face-background 'trailing-whitespace "#141700"))


(jl/set-frame-params 'initial-frame-alist) ; configure initial frame
(add-hook 'before-make-frame-hook ; configure new frames
          #'(lambda ()
              (jl/set-frame-params 'default-frame-alist)))


(use-package fringe
  :config
  (set-fringe-mode '(8 . 8)))

(use-package linum
  :config
  (global-linum-mode 1)
  (setq-default linum-format "%02d "))

(use-package hl-line
  :config
  (global-hl-line-mode 1) ; always highlight current line
  (setq-default hl-line-sticky-flag nil)) ; except in an inactive buffer
