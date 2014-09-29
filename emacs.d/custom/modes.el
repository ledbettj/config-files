;;; modes --- hooks for customizing various modes
;; John Ledbetter <john.ledbetter@gmail.com>

(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . js-mode))

; turn on auto-complete-mode pretty much everywhere it's not by default.
(mapc (lambda (mode) (add-to-list 'ac-modes mode))
  '(scss-mode css-mode rhtml-mode coffee-mode go-mode vala-mode rust-mode enh-ruby-mode
     text-mode fundamental-mode))

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
     (setq indent-tabs-mode nil)))

(add-hook 'vala-mode-hook
  '(lambda ()
     (setq indent-tabs-mode nil)))

(add-hook 'text-mode-hook
  '(lambda ()
     (flyspell-mode t)))

(add-hook 'dired-mode-hook 'rspec-dired-mode)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)
