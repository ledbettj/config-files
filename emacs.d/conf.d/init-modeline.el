(require 'all-the-icons)

; start stolen powerline code
(defun pl/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 for enabled and COLOR2 for disabled bits specified in DATA."
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (or color1 "None")
            (or color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar '(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar '(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun pl/percent-xpm
    (height pmax pmin winend winstart width color1 color2)
  "Generate percentage xpm."
  (let* ((height- (1- height))
         (fillstart (round (* height- (/ (float winstart) (float pmax)))))
         (fillend (round (* height- (/ (float winend) (float pmax)))))
         (data nil)
         (i 0))
    (while (< i height)
      (setq data (cons
                  (if (and (<= fillstart i)
                           (<= i fillend))
                      (append (make-list width 1))
                    (append (make-list width 0)))
                  data))
      (setq i (+ i 1)))
    (pl/make-xpm "percent" color1 color2 (reverse data))))
; end stolen powerline code

(set-face-attribute 'mode-line nil :height 0.9 :box nil)

(defface jl/not-modified-face
  '((t :foreground "#98be65" :height 0.9))
  "Modified file modeline icon"
  )

(defface jl/modified-face
  '((t :foreground "#ff6c6b" :height 0.9))
  "Not modified file modeline icon"
  )

(defface jl/read-write-face
  '((t :foreground "#969896"))
  "Read Write Buffer")

(defface jl/position-face
  '((t :height 0.9))
  "File position face")

(defvar jl/mode-line-modified
  '(:eval
     (if (buffer-modified-p (current-buffer))
        (all-the-icons-faicon "floppy-o" :height 1 :v-adjust 0 :face 'jl/modified-face)
       (all-the-icons-faicon "check" :height 1 :v-adjust 0 :face 'jl/not-modified-face ))))

(defvar jl/mode-line-bar '(:eval (propertize " "
                                             'display
                                             (if buffer-read-only
                                                 (pl/percent-xpm 20 100 0 0 0 6 "#0088CC" "#0088CC")
                                               (pl/percent-xpm 20 100 0 0 0 6 "#c678dd" "#c678dd")))))

(defvar jl/mode-line-ro '(:eval (if buffer-read-only
                                    (propertize "RO " 'face 'bold)
                                  (propertize "RW " 'face 'jl/read-write-face))))

(defvar jl/mode-line-buffer-identification
  '(:eval (propertize "%b" 'face 'bold)))

(defvar jl/mode-line-position
  '(:eval (propertize ":%l:%c %p " 'face 'jl/position-face)))

(defvar jl/mode-line-vc '(vc-mode ("   "
                                   (:eval (all-the-icons-faicon "code-fork"
                                                                :height 0.9
                                                                :v-adjust 0
                                                                :face (when (zerodark--active-window-p)
                                                                        (zerodark-git-face))))
                                   (:eval (propertize (truncate-string-to-width vc-mode 25 nil nil "...")
                                                      'face (when (zerodark--active-window-p)
                                                              (zerodark-git-face)))))))

(defvar jl/mode-line-modes
  '(
    #("%[" 0 2
      (help-echo "Recursive edit, type C-M-c to get out"))
    minor-mode-alist
    #("%]" 0 2
      (help-echo "Recursive edit, type C-M-c to get out"))
   " "))

(setq-default mode-line-format `(
    ,jl/mode-line-bar
    "%e"
    mode-line-front-space
    mode-line-mule-info
    ,jl/mode-line-ro
    "  "
    ,jl/mode-line-modified
    " "
    mode-line-auto-compile
    mode-line-remote
    mode-line-frame-identification
    mode-name
    " "
    ,jl/mode-line-buffer-identification
    ,jl/mode-line-position
    ,jl/mode-line-modes
    mode-line-misc-info
    mode-line-end-spaces
    ))

