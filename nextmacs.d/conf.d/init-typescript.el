(use-package tree-sitter :ensure t
    :mode (("\\.tsx\\'" . tsx-ts-mode)
           ("\\.ts\\'" . typescript-ts-mode))
    :hook (tsx-ts-mode . (lambda ()
                           (lsp)
                           (lsp-ui-mode)
                           ))
    :hook (typescript-ts-mode . (lambda ()
                           (lsp)
                           (lsp-ui-mode)
                           ))
    :hook (js-mode . (lambda ()
                           (lsp)
                           (lsp-ui-mode)
                           ))
    :config
    (setq lsp-apply-edits-after-file-operations nil)

    (setq treesit-font-lock-level 4)
    (setq treesit-language-source-alist
          '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
)
