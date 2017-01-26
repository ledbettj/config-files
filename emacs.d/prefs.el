;;; prefs.el
;;; --
;;; miscellaneous preferences, extracted out of other configuration files for
;;; ease of tweaking.


;; See (custom-available-themes) for the list of available themes.
(defconst prefs/theme          'monokai)
(defconst prefs/theme/terminal 'monokai) ; use me when running in a terminal.

;; preferred fonts, in order. the first one found on the system will be used.
(defconst prefs/font '(
  "Source Code Pro"
  "Hack"
  "Ubuntu Mono"
  "Monaco"))

(defconst prefs/font-size                  14) ; default font size.
(defconst prefs/font-size/display-laptop   12) ; font size to use on the builtin screen.
(defconst prefs/font-size/display-external 18) ; font size to use when docked.
