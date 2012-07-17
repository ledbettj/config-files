(add-to-list 'load-path "/usr/local/lib/node_modules/jshint-mode")

(if (require 'flymake-jshint nil t)
  (add-hook 'js-mode-hook
    '(lambda ()
       (flymake-mode t))))

(setq-default js-indent-level 2)
