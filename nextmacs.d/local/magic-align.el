(defun magic-align (BEG END)
  "Attempt to align the current region magically based on its contents."
  (interactive "r")
  (if (not (region-active-p))
      (message "mark not active")
    (let* ((body (buffer-substring-no-properties BEG END))
           (regex (cond
                   ((string-match-p "=>" body)               "\\(\\s-*\\)=>")
                   ((string-match-p "[^\s:]:\s" body)        ":\\(\\s-*\\)")
                   ((string-match-p "[^=><!]=[^=<>~]" body)  "\\(\\s-*\\)=")
                   (t nil))))
      (if regex
          (align-regexp BEG END regex 1 1)
        (message "no idea what you're trying to align.")))))

(provide 'magic-align)
