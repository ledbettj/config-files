;;;; keybinds.el - global key rebinds for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

(global-set-key [M-g]         'goto-line)
(global-set-key [M-up]        'move-text-up)
(global-set-key [M-down]      'move-text-down)
(global-set-key [M-return]    'toggle-fullscreen)
(global-set-key [C-kp-delete] 'kill-word)
(global-set-key [backspace]   'backward-delete-char-hungry)

