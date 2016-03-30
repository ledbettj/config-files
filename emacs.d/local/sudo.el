(defun reopen-file-with-sudo ()
  "Open the currently visited file as root via sudo."
  (interactive)
  (if (buffer-file-name)
      (let ((file-name (buffer-file-name)))
        (kill-buffer (current-buffer))
        (find-file (concat "/sudo::" file-name))
        (message "now editing %s as root" file-name))))

(provide 'sudo)
