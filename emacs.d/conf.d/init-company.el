(require 'use-package)

(use-package company-tern :ensure t :pin melpa)

(use-package company-quickhelp :ensure t :pin melpa
  :config
  (company-quickhelp-mode 1))

(use-package company :ensure t :pin melpa
  :config
  (setq-default company-lighter-base "Comp")
  (setq-default company-show-numbers          1)
  (setq-default company-idle-delay            0) ; start completion immediately
  (setq-default company-minimum-prefix-length 1) ; start completion after 1 character.
  (require 'company-dabbrev-code)
  (require 'company-keywords)
  (push 'enh-ruby-mode company-dabbrev-code-modes)
  (push  '(enh-ruby-mode
           "BEGIN" "END" "alias" "and"  "begin" "break" "case" "class" "def" "defined?"
           "do" "else" "elsif"  "end" "ensure" "false" "for" "if" "in" "module"
           "next" "nil" "not" "or" "redo" "rescue" "retry" "return" "self" "super"
           "then" "true" "undef" "unless" "until" "when" "while" "yield")
         company-keywords-alist)
  (global-company-mode 1))
