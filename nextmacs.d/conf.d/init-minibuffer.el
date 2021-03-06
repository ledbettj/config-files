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
  (setq ivy-height 16)
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  :init
  (ivy-mode t))

(use-package counsel :ensure t :defer t
  :config
  (setq counsel-find-file-ignore-regexp
        "\\(?:\\`[#.]\\)")
  :bind
  (("C-c s" . counsel-git-grep)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile :ensure t :defer t
  :config
  (counsel-projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))
