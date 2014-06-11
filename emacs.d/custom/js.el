;;; js.el - js-mode customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

(add-hook 'js-mode-hook
  (lambda ()
    (rainbow-mode)
    (tern-mode t)))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(setq-default js-indent-level 2)
