(unless (or (eq system-type 'darwin) t)
  (require 'svg-mode-line-themes)

  (defun smt/buffer-indicators-text (widget)
    (if buffer-read-only " RO " " RW "))

  (defun smt/buffer-name-text (widget)
    (format-mode-line "%b"))

  (defun smt/minor-mode-indicator-text (widget)
    (concat
      (when defining-kbd-macro                             " REC ")
      (when (bound-and-true-p projectile-mode)             " Proj ")
      (when (bound-and-true-p smartparens-mode)            " [S] ")
      (when (bound-and-true-p rainbow-mode)                " Rbow ")
      (when (bound-and-true-p rspec-mode)                  " Spec ")
      (when (bound-and-true-p flycheck-mode)               " Fly ")))

  (smt/defwidget buffer-dirty
    :text (lambda (widget)
            (if (and
                  (buffer-modified-p)
                  (or buffer-file-name buffer-offer-save))
              " ★ " " ✔ ")))

  (smt/defwidget position-info
    :text (lambda (widget)
            (format-mode-line "%l:%c [%p]"))
    :on-click (lambda (widget event)
                (what-cursor-position t)))

  (smt/defwidget major-mode
    :text (lambda (widget)
            (format-mode-line mode-name))
    :on-click (lambda (widget event)
                (message "%s" (format-mode-line mode-line-modes))))

  (smt/defrow default-position
    :widgets '(position-info)
    :align "right"
    :margin 5)

  (smt/defrow default-left
    :widgets '(buffer-info buffer-name buffer-dirty which-function)
    :margin 10)

  (smt/defrow default-right
    :widgets '(major-mode version-control minor-modes)
    :align "right"
    :margin 30)

  (defun jl:smt/background (theme)
    (let ((width (smt/window-pixel-width))
           (height (smt/t-pixel-height theme)))
      `((rect :width "100%" :height "100%" :x 0 :y 0 :fill "#121212" :fill-opacity 1)
         (rect :width "100%" :height 1 :x 0 :y height :fill "#383838" :fill-opacity 1)
         (g
           (polygon :points "0,0 6,0 22,16 6,32 0,32" :fill "blue")
                                        ;         (polygon :points "0,0 16,16 0,32" :fill "red")
           )
         )))


  (defun smt/jl-title-style (widget)
    (list :font-weight "normal"
      :font-size "8pt"
      :font-family "sans-serif"
      :filter nil
      :fill (if (smt/window-active-p)
              "#FFFFFF"
              "#666666")))

  (defun smt/jl-major-mode-style (widget)
    (list :font-weight "normal"
      :font-size "10pt"
      :filter nil
      :font-family "sans-serif"
      :fill (if (smt/window-active-p)
              "#AAAAAA"
              "#666666")))

  (defun smt/jl-info-style (widget)
    (list :font-weight "normal"
      :font-size "6pt"
      :filter nil
      :font-family "sans-serif"
      :fill (if (smt/window-active-p)
              "#999999"
              "#555555")))

  (defun smt/jl-position-info-style (widget)
    (list :font-weight "normal"
      :font-size "8pt"
      :filter nil
      :fill (if (smt/window-active-p)
              "#DDDDDD"
              "#999999")))

  (defun smt/jl-dirty-style (widget)
    (list :font-weight "normal"
      :font-size "11pt"
      :filter nil
      :font-family "sans-serif"
      :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
              ;; Dirty
              (if (smt/window-active-p)
                "#FF6060" "#763030")
              ;; Untouched
              (if (smt/window-active-p)
                "#1F4F25" "#143519"))))

  (defun smt/jl-minor-mode-style (widget)
    (list :font-weight "normal"
      :font-size "8pt"
      :filter nil
      :fill (if (smt/window-active-p)
              "#666"
              "#333")))

  (defun smt/jl-version-control-style (widget)
    (list :font-weight "bold"
      :font-size "8pt"
      :filter nil
      :font-family "sans-serif"
      :fill (if (smt/window-active-p)
              "#60B18C"
              "#666666")))

  (smt/deftheme jl:smt
    :pixel-height 32
    :background 'jl:smt/background
    :local-widgets
    (list (cons 'major-mode
            (smt/make-widget
              :prototype 'major-mode
              :style 'smt/jl-major-mode-style))

      (cons 'minor-modes
        (smt/make-widget
          :prototype 'minor-modes
          :style 'smt/jl-minor-mode-style))

      (cons 'version-control
        (smt/make-widget
          :prototype 'version-control
          :style 'smt/jl-version-control-style))

      (cons 'position-info
        (smt/make-widget
          :prototype 'position-info
          :style 'smt/jl-position-info-style))

      (cons 'buffer-info
        (smt/make-widget
          :prototype 'buffer-info
          :style 'smt/jl-info-style))

      (cons 'buffer-dirty
        (smt/make-widget
          :prototype 'buffer-dirty
          :style 'smt/jl-dirty-style))

      (cons 'buffer-name
        (smt/make-widget
          :prototype 'buffer-name
          :style 'smt/jl-title-style)))

    :rows (list
            'default-left
            'default-position
            'default-right))


  (let ((theme (cdr (assoc 'archetype smt/themes)))
         (row (cdr (assoc 'archetype smt/rows))))

    ;; Customise to use your desired default font
    (setf (getf theme :style) (list :font-size "10pt" :font-family "Ubuntu"))

    (setf (getf row :baseline) 19))

  (smt/enable)
  (smt/set-theme 'jl:smt)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))
