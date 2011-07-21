
;;; smoove.el 
;; Copyright (C) 2010 John Ledbetter <john.ledbetter@gmail.com>

(require 'color-theme)
(defun color-theme-smoove ()
  (interactive)
  (color-theme-install
   '(smoove
      ((background-color . "#f0f0f0")
      (background-mode . light)
      (border-color . "#969696")
      (cursor-color . "#f5e92e")
      (foreground-color . "#000000")
      (mouse-color . "black"))
     (fringe ((t (:background "#969696"))))
     (mode-line ((t (:foreground "#000000" :background "#8f8f8f"))))
     (region ((t (:background "#4b87d8"))))
     (font-lock-builtin-face ((t (:foreground "#000000"))))
     (font-lock-comment-face ((t (:foreground "#009400"))))
     (font-lock-function-name-face ((t (:foreground "#000000"))))
     (font-lock-keyword-face ((t (:foreground "#1523c1"))))
     (font-lock-string-face ((t (:foreground "#9b0811"))))
     (font-lock-type-face ((t (:foreground"#000000"))))
     (font-lock-variable-name-face ((t (:foreground "#000000"))))
     (minibuffer-prompt ((t (:foreground "#000000" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))

(defalias 'smoove #'color-theme-smoove)

(provide 'smoove)



