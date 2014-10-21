(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
          (column (c-langelem-2nd-pos c-syntactic-element))
          (offset (- (1+ column) anchor))
          (steps (floor offset c-basic-offset)))
    (* (max steps 1)
      c-basic-offset)))

(add-hook 'c-mode-common-hook
  (lambda ()
    ;; Add kernel style
    (c-add-style
      "linux-tabs-only"
      '("linux" (c-offsets-alist
                  (arglist-cont-nonempty
                    c-lineup-gcc-asm-reg
                    c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
  (lambda ()
    (local-set-key (kbd "RET") 'newline-and-indent)
    (setq flycheck-gcc-language-standard "c99")
    (setq flycheck-clang-language-standard "c99")
    (let ((filename (buffer-file-name)))
      ;; Enable kernel mode for files in a linux directory
      (when (and filename
              (or
                (string-match "/linux/" filename)
                (string-match "/qemu-strap/" filename)))
        (setq indent-tabs-mode t)
        (setq tab-width 8)
        (c-set-style "linux-tabs-only")))))
