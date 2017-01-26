(use-package projectile :ensure t :pin melpa
  :diminish " Ⓟ"
  :init
  :config
  (projectile-global-mode)
  (setq projectile-mode-line " Ⓟ"))

(use-package helm-projectile :ensure t :pin melpa
  :bind (("C-c f" . helm-projectile-find-file)
         ("C-c s" . helm-project-grep)  ; this is ctrl+command+space on OS X
         ([(control super ?\ )] . helm-project-grep)) ; this is ctrl+command+space on Linux (sane, right?)
  :config
  (defun helm-project-grep ()
    "search all files in the project."
    (interactive)
    (helm-grep-git-1 (projectile-expand-root "."))))
