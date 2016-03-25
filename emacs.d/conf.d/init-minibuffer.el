(require 'use-package)

(use-package simple
  :init
  (savehist-mode 1)
  (setq history-length 1000)
  (setq minibuffer-prompt-properties
	(plist-put minibuffer-prompt-properties 'point-entered
		   'minibuffer-avoid-prompt)))
