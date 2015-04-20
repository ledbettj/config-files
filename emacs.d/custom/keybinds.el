;;;; keybinds.el - global key rebinds for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

(global-set-key (kbd "M-g")   'goto-line)
(global-set-key [M-up]        'move-text-up)
(global-set-key [M-down]      'move-text-down)
(global-set-key [M-return]    'toggle-fullscreen)
(global-set-key [C-kp-delete] 'kill-word)
(global-set-key [backspace]   'backward-delete-char-hungry)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-<next>") (lambda() (interactive) (next-line 10)))
(global-set-key (kbd "C-<prior>") (lambda() (interactive) (previous-line 10)))
(global-set-key (kbd "C-c a") 'magic-align)
(global-set-key (kbd "C-c C-s") 'reopen-file-with-sudo)

(when (eq system-type 'darwin)
  (global-set-key [C-s-268632064] 'helm-project-grep) ; ctrl-cmd-spc
  (global-set-key [C-s-268632083] 'helm-multi-occur-all)) ; ctrl-cmd-s

(unless (eq system-type 'darwin)
  (global-set-key (kbd "C-s-SPC") 'helm-project-grep))
;  (global-set-key (kbd "C-s-SPC") 'helm-multi-occur-all))

; make right and left arrow pick matching buffers in iswitchb-mode
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
    '(("<right>" . iswitchb-next-match)
       ("<left>"  . iswitchb-prev-match))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

; treat fn as control on OS X since it lives where my control key should live
(if (eq system-type 'darwin)
  (setq-default ns-function-modifier 'control))

