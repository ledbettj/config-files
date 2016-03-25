; greedy whitespace delete
(defun hungry-delete-backwards (arg &optional killp)
  "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and
    `backward-delete-char-untabify-method' for details."
  (interactive "*p\nP")
  (let ((backward-delete-char-untabify-method 'hungry))
    (backward-delete-char-untabify arg killp)))

(provide 'hungry-delete)
