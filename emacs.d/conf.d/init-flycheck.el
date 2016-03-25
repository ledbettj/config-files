(require 'use-package)

(use-package flycheck :ensure t :pin melpa
  :config
  (setq-default flycheck-indication-mode nil)
  (setq-default flycheck-navigation-minimum-level 'warning)
  (global-flycheck-mode)
  (setq flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            (`not-checked " ?")
            (`no-checker (propertize " -" 'face 'warning))
            (`running (propertize " ❂" 'face 'escape-glyph))
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
            (`suspicious '(propertize " ⚠" 'face 'warning))))))
