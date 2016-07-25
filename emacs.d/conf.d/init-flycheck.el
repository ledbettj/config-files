(use-package flycheck :ensure t :pin melpa
  :config
  (setq-default flycheck-indication-mode nil)
  (setq-default flycheck-navigation-minimum-level 'warning)
  (setq flycheck-checkers (delete 'emacs-lisp-checkdoc flycheck-checkers))
  (global-flycheck-mode)
  (setq flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            (`not-checked " ?")
            (`no-checker (propertize " -" 'face 'font-lock-comment-face))
            (`running (propertize " ◌" 'face 'escape-glyph))
            (`errored (propertize " ⚠" 'face 'error))
            (`finished
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (propertize (if (or no-errors no-warnings) " ✘" " ✔") 'face face)))
            (`interrupted " ⚠")
            (`suspicious '(propertize " ⚠" 'face 'warning)))))

  (defhydra hydra-flycheck ()
    "Errors"
    ("n" flycheck-next-error                                       "Next")
    ("p" flycheck-previous-error                                   "Previous")
    ("f" flycheck-first-error                                      "First")
    ("l" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q" nil))
  (global-set-key (kbd "C-c e") 'hydra-flycheck/body))
