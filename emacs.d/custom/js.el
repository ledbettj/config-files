;;; js.el - js-mode customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>
(require 'flymake-node-jshint)

(if (executable-find "jshint")
  (add-hook 'js-mode-hook
    '(lambda ()
       (flymake-mode t))))

(setq-default js-indent-level 2)
