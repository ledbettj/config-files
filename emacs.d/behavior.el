;;; behavior.el - customization for varios emacs behavior
;; John Ledbetter <john.ledbetter@gmail.com>

(setq kill-whole-line t) ; include EOL when killing lines
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; always ask Y/N? instead of yes/no.

(defun toggle-fullscreen ()
  "Switch between fullscreen and windowed mode"
  (interactive)
  (if (eq system-type 'darwin)
      (ns-toggle-fullscreen) ; OS-X
    (set-frame-parameter nil 'fullscreen
			 (if (frame-parameter nil 'fullscreen) nil 'fullboth))))

;; bind alt+enter to fullscreen mode
(global-set-key (kbd "M-RET") 'toggle-fullscreen)
