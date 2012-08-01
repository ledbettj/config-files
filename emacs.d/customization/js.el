;;; js.el - js-mode customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

(let ((possible-jshint-paths '("/usr/lib/node_modules/jshint-mode"
                                "/usr/local/lib/node_modules/jshint-mode")))
  (loop for path in possible-jshint-paths do
    (if (file-exists-p path)
      (add-to-list 'load-path path))))

(if (require 'flymake-jshint nil t)
  (add-hook 'js-mode-hook
    '(lambda ()
       (flymake-mode t))))

(setq-default js-indent-level 2)
