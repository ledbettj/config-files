(defvar jl/theme-initialized nil)

(use-package doom-themes :ensure t)

;; setup a frame with the appropriate UI settings.
;; this is either initial-frame-alist to apply to the already created initial frame,
;; or default-frame-alist to setup new frames.
(defun jl/set-frame-params (frame-alist)
  (add-to-list frame-alist
               `(font .
                      ,(concat prefs/use-font "-"
                               (number-to-string
                                (let ((px (display-pixel-width)))
                                  (cond
                                   ((eq px 7040) prefs/font-size/display-external)
                                   ((eq px 3200) prefs/font-size/display-laptop)
                                   (t prefs/font-size)))))))
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
    (load-theme prefs/use-theme)))


(jl/set-frame-params 'initial-frame-alist) ; configure initial frame
(add-hook 'before-make-frame-hook ; configure new frames
          #'(lambda ()
              (jl/set-frame-params 'default-frame-alist)))
