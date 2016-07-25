(use-package hydra :ensure t)

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("_" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c z") 'hydra-zoom/body)
