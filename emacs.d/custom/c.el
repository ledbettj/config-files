(when (featurep 'flycheck-autoloads)
  (require 'flycheck)
  (setq flycheck-highlighting-mode 'lines)
  (defun make-clang-pattern (str level)
    (list (concat "^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): " str ": +\\(?4:.*\\)$") level))

  (flycheck-declare-checker c-clang-syntax
    "Check file using clang's syntax checker."
    :command '("clang"
               "-fsyntax-only"
               "-fno-diagnostics-show-option"
               "-fno-caret-diagnostics"
               source-inplace)
    :error-patterns
    (list (make-clang-pattern "warning" 'warning)
          (make-clang-pattern "note" 'warning)
          (make-clang-pattern "error" 'error)
          (make-clang-pattern "fatal error" 'error))
    :modes '(c-mode c++-mode cc-mode))
  (add-to-list 'flycheck-checkers 'c-clang-syntax))
