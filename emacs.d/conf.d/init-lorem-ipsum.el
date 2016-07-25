(use-package lorem-ipsum :ensure t :pin melpa
  :init
  (defhydra hydra-lorem-ipsum ()
    "lorem ipsum"
    ("l" lorem-ipsum-insert-list "list")
    ("p" lorem-ipsum-insert-paragraphs "paragraph")
    ("s" lorem-ipsum-insert-sentences "sentence")
    ("q" nil "quit" :color blue))
  (global-set-key (kbd "C-c l") 'hydra-lorem-ipsum/body))
