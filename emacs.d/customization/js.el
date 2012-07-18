;;; js.el - js-mode customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

; try to load jshint flymake integration
(add-to-list 'load-path "/usr/local/lib/node_modules/jshint-mode")

(if (require 'flymake-jshint nil t)
  (add-hook 'js-mode-hook
    '(lambda ()
       (flymake-mode t))))

(setq-default js-indent-level 2)
