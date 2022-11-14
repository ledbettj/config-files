;;; prefs.el
;;; --
;;; miscellaneous preferences, extracted out of other configuration files for
;;; ease of tweaking.


;; See (custom-available-themes) for the list of available themes.
(defconst prefs/theme          'doom-dracula)
(defconst prefs/theme/terminal 'doom-dracula) ; use me when running in a terminal.

;; preferred fonts, in order. the first one found on the system will be used.
(defconst prefs/font '(
;  "Fira Code Retina"
                                        ;  "Roboto Mono"
  "JetBrains Mono"
  "Source Code Pro"
  "Hack"
  "Ubuntu Mono"
  "Monaco"))

(defconst prefs/ensure-paths
  `(
    (prepend . ,(expand-file-name "~/.gem/ruby/3.0.0/bin"))
    (prepend . ,(expand-file-name "~/.rbenv/bin"))
    (prepend . ,(expand-file-name "~/.rbenv/shims"))
    (prepend . ,(expand-file-name "~/Local/bin"))
    (prepend . ,(expand-file-name "~/.nvm/versions/node/v14.18.3/bin"))
    (append .  "/usr/local/bin")
    (append . ,(expand-file-name "~/.yarn/bin"))
    (append . ,(expand-file-name "~/.cargo/bin"))
    (append . ,(expand-file-name "~/go/bin"))
    (append . ,(expand-file-name "~/Projects/Android/emulator"))
    (append . ,(expand-file-name "~/Projects/Android/tools"))
    (append . ,(expand-file-name "~/Projects/Android/tools/bin"))
    (append . ,(expand-file-name "~/Projects/Android/platform-tools"))))

;; display pixel width and corresponding font size.
(defconst prefs/font-size
  '(( 7040 . 18 )
    ( 6400 . 18 )
    ( 5968 . 18 )
    ( 3200 . 18 )
    ( 2560 . 14 )
    ( 2128 . 18 )))

(defconst prefs/default-font-size 16)

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
  (let ((px (display-pixel-width)))
    (or
     (cdr (assoc px prefs/font-size))
     prefs/default-font-size)))
