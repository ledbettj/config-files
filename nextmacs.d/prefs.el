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

(defconst prefs/ensure-paths
  `(
    (prepend . ,(expand-file-name "~/.gem/ruby/2.6.0/bin"))
    (prepend . ,(expand-file-name "~/.rbenv/bin"))
    (prepend . ,(expand-file-name "~/.rbenv/shims"))
    (append .  "/usr/local/bin")))

;; display pixel width and corresponding font size.
(defconst prefs/font-size
  '(( 7040 . 18 )
    ( 6400 . 18 )
    ( 3200 . 16 )
    ( 2128 . 16 )))

(defconst prefs/default-font-size 14)

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

(defun prefs/use-font-size ()
  (let ((px (x-display-pixel-width ":0")))
    (or
     (cdr (assoc px prefs/font-size))
     prefs/default-font-size)))
