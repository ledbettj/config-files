;;; js.el - js-mode customizations for emacs
;; John Ledbetter <john.ledbetter@gmail.com>

(add-hook 'js-mode-hook
  (lambda ()
    (rainbow-mode)
    (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (add-to-list 'company-backends 'company-tern)))

(setq-default js-indent-level 2)
