(require 'use-package)

(use-package projectile :ensure t :pin melpa
  :init
  :config
  (projectile-global-mode)
  (setq projectile-mode-line '(:eval (format " P[%s]"
                                             (projectile-project-name)))))

(use-package projectile-rails :ensure t :pin melpa
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  :config
  (setq projectile-rails-add-keywords nil))

(use-package helm-projectile :ensure t :pin melpa
  :bind (("C-c f" . helm-projectile-find-file)
         ([C-s-268632064] . helm-project-grep)  ; this is ctrl+command+space on OS X
         ([C-s- ]         . helm-project-grep)) ; this is ctrl+command+space on Linux (sane, right?)
  :config
  (defun helm-project-grep ()
    "search all files in the project."
    (interactive)
    (helm-grep-git-1 (projectile-expand-root "."))))
