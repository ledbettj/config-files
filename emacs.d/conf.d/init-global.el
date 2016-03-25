(require 'use-package)

(use-package hungry-delete
  :load-path "local/"
  :bind (([remap backward-delete-char-untabify] . hungry-delete-backwards)))
