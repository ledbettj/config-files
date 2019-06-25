(defun reopen-file-with-sudo ()
  "Open the currently visited file as root via sudo."
  (interactive)
  (if (buffer-file-name)
      (let ((file-name (buffer-file-name))
            (offset (point))
            (old-buffer (current-buffer)))
        (find-file (concat "/sudo::" file-name))
        (goto-char offset)
        (message "now editing %s as root" file-name)
        (kill-buffer old-buffer))))

(provide 'sudo)



;; (get-buffer-process (current-buffer))


;; (eq (process-buffer) (current-buffer))
