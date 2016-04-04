;;; prefs.el
;;; --
;;; miscellaneous preferences, extracted out of other configuration files for
;;; ease of tweaking.


;; See (custom-available-themes) for the list of available themes.
(defconst prefs/theme 'solarized-dark)

;; preferred fonts, in order. the first one found on the system will be used.
(defconst prefs/font '(
  "Hack"
  "Source Code Pro"
  "Ubuntu Mono"
  "Monaco"))

(defconst prefs/font-size                  12) ; default font size.
(defconst prefs/font-size-macbook-builtin  16) ; font size to use on the builtin mac screen.
(defconst prefs/font-size-macbook-external 18) ; font size to use when docked.
