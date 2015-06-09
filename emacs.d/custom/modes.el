;;; modes --- hooks for customizing various modes
;; John Ledbetter <john.ledbetter@gmail.com>
(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . js-mode))

(add-hook 'markdown-mode-hook
  '(lambda ()
     (flyspell-mode t)))

(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (setq lisp-indent-offset 2)
     (rainbow-mode t)))

(add-hook 'scss-mode-hook
  '(lambda ()
     (setq scss-compile-at-save nil)
     (setq css-indent-offset 2)
     (rainbow-mode t)))

(add-hook 'css-mode-hook
  '(lambda ()
     (setq css-indent-offset 2)
     (rainbow-mode t)))

(add-hook 'rhtml-mode-hook
  '(lambda ()
     (rainbow-mode t)))

(add-hook 'sh-mode-hook
  '(lambda ()
     (setq sh-basic-offset 2)))

(add-hook 'coffee-mode-hook
  '(lambda ()
     (make-local-variable 'tab-width)
     (set 'tab-width 2)))

(add-hook 'go-mode-hook
  '(lambda ()
     (local-set-key (kbd "C-c f") 'gofmt)
     (setq tab-width 4 indent-tabs-mode t)))

(add-hook 'vala-mode-hook
  '(lambda ()
     (setq indent-tabs-mode nil)))

(add-hook 'text-mode-hook
  '(lambda ()
     (flyspell-mode t)))

(add-hook 'dired-mode-hook 'rspec-dired-mode)
(add-hook 'enh-ruby-mode-hook 'rspec-mode)
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js-mode-hook 'fic-mode)
(add-hook 'rust-mode-hook 'fic-mode)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)
