(require 'all-the-icons)

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

