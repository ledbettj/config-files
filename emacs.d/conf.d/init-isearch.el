(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook #'endless/goto-match-beginning)

