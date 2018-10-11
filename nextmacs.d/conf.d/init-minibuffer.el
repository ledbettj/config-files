(savehist-mode 1)
(setq history-length 1000)
(setq minibuffer-prompt-properties
      (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

(use-package swiper :ensure t :defer t
  :bind
  (("C-s" . 'swiper)))

(use-package ivy :ensure t :defer t
  :bind
  (:map ivy-minibuffer-map
        ("RET" . 'ivy-alt-done)
        ("C-j" . 'ivy-immediate-done))
  :config
  (setq ivy-extra-directories nil)
  :init
  (ivy-mode t))

(use-package counsel :ensure t :defer t
  :bind
  (("C-c s" . counsel-git-grep)))
