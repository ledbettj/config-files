(nconc load-path
       (list "~/.emacs.d/site-lisp"       "~/.emacs.d/packages/yasnippet"
             "~/.emacs.d/packages/nxhtml" "~/.emacs.d/packages/color-theme"
             "~/.emacs.d/packages/slime"  "~/.emacs.d/packages/autocomplete"))

(require 'color-theme)
(require 'zenburn)
(require 'flymake)
(require 'auto-complete-config)
(require 'xcscope)
(require 'cl)
(require 'lua-mode)
(require 'markdown-mode)
(require 'rvm)
(require 'yaml-mode)
(require 'sass-mode)
(require 'js2-mode)
(require 'uniquify)
(require 'haml-mode)
(require 'vc)
(require 'coffee-mode)
(require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/packages/nxhtml/autostart.el")
(setq debug-on-error nil)
(load-file "~/.emacs.d/packages/color-theme/themes/color-theme-wombat.el")
(color-theme-wombat)

(setq inferior-lisp-program "/usr/local/bin/clisp")
(require 'slime)
(slime-setup '(slime-fancy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize yanippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)
(setq yas/trigger-key (kbd "C-c <kpd-multiply>")) ; ac will handle this for us.
(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-g"      'goto-line)
(global-set-key [C-prior]   'previous-buffer)
(global-set-key [C-next]    'next-buffer)
(global-set-key [C-tab]     'toggle-tabs-mode)
(global-set-key [backtab]   'toggle-tab-width)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 'align-repeat)

(if (eq system-type 'darwin)
    (global-set-key "\M-\r" 'ns-toggle-fullscreen)
    ; else
    (global-set-key "\M-\r"     'toggle-fullscreen)
)

(if (eq system-type 'darwin)
    (progn 
      (global-set-key (kbd "<kp-delete>") 'delete-char)
      (global-set-key (kbd "C-<kp-delete>") 'kill-word)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(iswitchb-mode               t)   ; better C-x b buffer switching
(setq kill-whole-line        t)   ; C-k removes newline as well
(setq next-line-add-newlines nil) ; don't add newlines when past end of buffer
(setq require-final-newline  nil) ; don't require \n at end of buffer
(delete-selection-mode       t)   ; delete selection when delete is pressed
(setq transient-mark-mode    t)   ; handle selections sanely
(setq fill-column            80)  ; when filling text, fill 80 characters/line
(setq-default indent-tabs-mode       nil) ; use spaces for indentation
(fset 'yes-or-no-p     'y-or-n-p) ; ask y/n instead of yes/no
(setq scroll-conservatively  1)   ; better scrolling behavrio
(setq inhibit-startup-message t)  ; don't show emacs screen at startup
(which-function-mode          t)  ; show function name in modeline
(add-to-list                      ; turn on auto complete mode
     'ac-dictionary-directories
     "/usr/share/emacs23/site-lisp/ac-dict")
(ac-config-default)
(put 'downcase-region 'disabled nil) ; don't disble these commands,
(put 'upcase-region   'disabled nil) ; I think they're useful
(show-paren-mode)
(rvm-use-default)                    ; set up ruby / gems using RVM default
(setq uniquify-buffer-name-style 'forward) ; better unique buffer naming
(setq uniquify-after-kill-buffer-p t) ; remove uniquify name after killing 
                                      ; a competing buffer.
(setq mumamo-chunk-coloring 2)        ; god this is ugly turn it off turn it off
(setq-default show-trailing-whitespace t) ; highlight trailing whitespace
(setq-default js2-bounce-indent-p nil) ; don't use frustrating tab indent
(setq-default js2-basic-offset 2)      ; basic indent is 2 spaces
(setq-default tab-width  4)   ; default tab width is 4 spaces
(setq-default c-basic-offset 4)   ; yes, still 4 spaces
(setq-default css-indent-offset 2) ; 2 space indent in css

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append
       (list
        '("\\.lua$"          .  lua-mode)
        '("\\.md$"           .  markdown-mode)
        '("\\.cnote-theme$"  .  js2-mode)
        '("\\.json$"         .  js2-mode)
        '("Gemfile$"         .  ruby-mode)
        '("Rakefile$"        .  ruby-mode)
        '("\\.gemspec$"      .  ruby-mode)
        '("\\.rake$"         .  ruby-mode)
        '("\\.yml$"          .  yaml-mode)
        '("\\.yaml$"         .  yaml-mode)
        '("\\.js$"           .  js2-mode)
        '("\\.haml$"         .  haml-mode)
        '("\\.scss$"         .  css-mode)
        '("\\.html\\.erb$"   .  eruby-nxhtml-mumamo-mode)
        '("\\.coffee$"       .  coffee-mode)
        '("Cakefile$"        .  coffee-mode))
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 140 :family "Droid Sans Mono")
    ; else
    (set-face-attribute 'default nil :height 100 :family "Bitstream Vera Sans Mono")
)

(global-font-lock-mode   t)   ; turn on decorations in all modes
(menu-bar-mode           1)
(tool-bar-mode           0)   ; hide toolbar and scroll bar
(scroll-bar-mode         0)
(if (eq system-type 'darwin)
    (setq visible-bell nil)
    ;else use flashing buffer instead of audible beep
    (setq visible-bell t)
)
(setq line-number-mode   t)   ; show line and column in mode line
(setq column-number-mode t)
(display-time-mode       nil) ; hide time in mode line

(setq frame-title-format      ; show 'user@host: buffername*' as frame title
      (list (user-login-name)
            "@" (system-name)
            ": %b %+" ))
(setq truncate-partial-width-windows nil) ; no line truncating in split windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save backup files in a non-annoying place
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar backup-directory-location "~/.cache/emacs")
(setq backup-directory-alist
      `((".*" . ,backup-directory-location)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-directory-location t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun toggle-tabs-mode ()
  "Toggle variable indent-tabs-mode between t and nil."
  (interactive)
  (set-variable 'indent-tabs-mode (not indent-tabs-mode))
  (message "tabs-mode set to %s" indent-tabs-mode))

(defun toggle-tab-width ()
  "Toggle variable tab-width between 8 and 4."
  (interactive)
  (set-variable 'tab-width (if (= tab-width 8) 4 8))
  (message "tab-width set to %d" tab-width))

(defun toggle-fullscreen ()
  "Switch between fullscreen and windowed mode"
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
										   nil
										 'fullboth)))

(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun hexcolour-add-to-font-lock ()
  "Colorize HTML RGB colors (e.g. '#0000F0', 'blue') in font-lock-mode."
  (interactive)
  (font-lock-add-keywords nil
	`((,(concat "#[0-9a-fA-F]\\{6\\}\\|"
                (regexp-opt (x-defined-colors) 'words))
	   (0 (let ((colour (match-string-no-properties 0)))
            (put-text-property
             (match-beginning 0) (match-end 0)
             'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                       "white" "black"))
                     (:background ,colour)))))))))

(defun c-return ()
  "indent automatically on return"
  (interactive)
  (c-indent-line-or-region)
  (newline-and-indent))

(defun cish-lang-hook ()
  " setup C-ish mode (C/C++)"
  (c-set-style   "k&r")
  (local-set-key [13] 'c-return)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset  'substatement-open  0) ; no indent for opening {
  (show-paren-mode 1) ; highlight matching parens
  (c-toggle-auto-hungry-state 1) ; insert newlines when appropriate
										; and do greedy whitespace delete
  (flymake-mode t) ; turn on flymake
)

(defun javascript-hook ()
  "javascript blows, I hate it."
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (hexcolour-add-to-font-lock)
)

(defun on-text-mode ()
  "turn on flyspell"
  (flyspell-mode t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'css-mode-hook  'hexcolour-add-to-font-lock)
(add-hook 'nxml-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'sass-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'emacs-lisp-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'c-mode-hook    'cish-lang-hook)
(add-hook 'c++-mode-hook  'cish-lang-hook)
(add-hook 'text-mode-hook 'on-text-mode)
(add-hook 'js2-mode-hook 'javascript-hook)
(add-hook 'window-setup-hook 'delete-other-windows) ; only one window on startup


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake crap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (load "flymake" t) 
  ; don't show a goddamn dialog box every single time flymake cocks up
  (defun flymake-display-warning (warning) 
	"Display a warning to the user, using lwarn"
	(message warning))


  ; turn on flymake mode for python using pyflakes
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
		       'flymake-create-temp-inplace)) 
	   (local-file (file-relative-name 
			temp-file 
			(file-name-directory buffer-file-name)))) 
      (list "pychecker" (list local-file)))))
  
(add-to-list 'flymake-allowed-file-name-masks 
	     '("\\.py\\'" flymake-pyflakes-init)) 

(add-hook 'find-file-hook 'flymake-find-file-hook)

; use chtex for tex instead of texify
(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

; set up flymake for ruby
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))

; no ispell on OS X; use aspell for flyspell mode
(if (eq system-type 'darwin)
    (custom-set-variables
     '(ispell-program-name "/usr/local/bin/aspell")
     '(safe-local-variable-values (quote ((encoding . utf-8)))))
)

(set-face-attribute 'js2-error-face nil
		    :background "#4c0303")
(set-face-attribute 'js2-warning-face nil
		    :background "#6c3303")
(set-face-attribute 'trailing-whitespace nil
            :background "#202020")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make uniquify place nice with iswitchb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan)
)

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get rid of annoying 'deprecated function' warnings from mumamo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nifty 'move line/region up/down' command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
