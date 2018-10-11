;;; prefs.el
;;; --
;;; miscellaneous preferences, extracted out of other configuration files for
;;; ease of tweaking.


;; See (custom-available-themes) for the list of available themes.
(defconst prefs/theme          'doom-vibrant)
(defconst prefs/theme/terminal 'doom-vibrant) ; use me when running in a terminal.

;; preferred fonts, in order. the first one found on the system will be used.
(defconst prefs/font '(
  "Source Code Pro"
  "Hack"
  "Ubuntu Mono"
  "Monaco"))

(defconst prefs/font-size                  14) ; default font size.
(defconst prefs/font-size/display-laptop   12) ; font size to use on the builtin screen.
(defconst prefs/font-size/display-external 18) ; font size to use when docked.



;; helper functions go down here, prefs go up there.

(defun font-exists-p (font)
  "check if the specified font is present on the system"
  (if (null (x-list-fonts font)) nil t))

(defconst prefs/use-font
  (if (display-graphic-p)
      (cl-find-if 'font-exists-p prefs/font)
    (car prefs/font)))

(defconst prefs/use-theme
  (if (display-graphic-p)
      prefs/theme
    prefs/theme/terminal))
