;;; behavior.el - customization for varios emacs behavior
;; John Ledbetter <john.ledbetter@gmail.com>

(setq kill-whole-line t) ; include EOL when killing lines
(setq-default indent-tabs-mode nil) ; never use tabs for indenting
(iswitchb-mode t) ; enhanced buffer switching
(delete-selection-mode t) ; when region is active, delete kills region

(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; always ask Y/N? instead of yes/no.

; stop writing annoying autosave files to the current directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun toggle-fullscreen ()
  "Switch between fullscreen and windowed mode"
  (interactive)
  (if (eq system-type 'darwin)
      (ns-toggle-fullscreen) ; OS-X
    (set-frame-parameter nil 'fullscreen
      (if (frame-parameter nil 'fullscreen) nil 'fullboth))))

; iswitchb customization
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquifiy' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-makealist iswitchb-default)
  (setq iswitchb-rescan t))

(setq iswitchb-buffer-ignore
  '("^ " "^\\*"))
