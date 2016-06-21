(use-package lorem-ipsum :ensure t :pin melpa
  :init
  (defhydra hydra-lorem-ipsum (global-map "C-c l")
    "lorem ipsum"
    ("l" lorem-ipsum-insert-list)
    ("p" lorem-ipsum-insert-paragraphs)
    ("s" lorem-ipsum-insert-sentences)
    ("q" nil "quit" :color blue)
    ))
