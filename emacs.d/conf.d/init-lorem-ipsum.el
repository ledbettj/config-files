(use-package lorem-ipsum :ensure t :pin melpa
  :bind (
         ("C-c l l" . lorem-ipsum-insert-lists)
         ("C-c l p" . lorem-ipsum-insert-paragraphs)
         ("C-c l s" . lorem-ipsum-insert-sentences)))
