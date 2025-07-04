;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq tab-always-indent t)
(setq kill-whole-line t)
(setq confirm-kill-emacs nil)
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq lsp-log-io nil) ; if set to true can cause a performance hit
(setq-hook! 'ruby-mode-hook +format-with 'ruby-standard)
(setq +format-on-save-enabled-modes
      '(rust-mode elixir-mode))
(+global-word-wrap-mode +1)
;; stop accidentally zooming like mad
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(rust-mode 2))
  (add-to-list 'copilot-indentation-alist '(python-mode 2))
  (add-to-list 'copilot-indentation-alist '(typescript-mode 2))
  (add-to-list 'copilot-indentation-alist '(ruby-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package! company
  :config
  (setq-default lsp-completion-provider           :capf)
  (setq-default company-idle-delay                0.5)
  (setq-default company-minimum-prefix-length     2)
  (setq-default company-show-numbers              1)
  (setq-default company-tooltip-align-annotations t))

(use-package! iedit
  :defer
  :bind ("C-;" . iedit-mode))

(use-package! hungry-delete
  :config
  (setq-default hungry-delete-join-reluctantly t)
  :hook (prog-mode . hungry-delete-mode))

(use-package! protobuf-mode
  :defer-incrementally t)

(use-package! sudo
  :defer
  :bind (("C-c C-s" . reopen-file-with-sudo)))

(use-package! flycheck
  :config
  ;; use flycheck-standardrb for ruby files
  (setq-default flycheck-disabled-checkers
                '(ruby-rubocop)))

(use-package! sh-script
  :mode ("\\.env" . bash-ts-mode)) ; note this doesn't include a trailing ' so it also matches '.env.local' etc

(use-package! rust-mode
  :config
  (setq rust-format-on-save t)
  (setq rust-indent-offset 2)
  )

(use-package! claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c z" . claude-code-command-map))

(use-package! rubocop
  :config
  (setq rubocop-autocorrect-command "standardb -a --format emacs")
  (setq rubocop-format-command "standardrb -x --format emacs")
  (setq rubocop-check-command "standardrb --format emacs"))

(use-package! apheleia
  :config
  (setf (alist-get 'rubocop apheleia-formatters)
        (alist-get 'ruby-standard apheleia-formatters))
  (setf
   (alist-get 'ruby-mode apheleia-mode-alist) '(ruby-standard)))


(if (eq (window-system) 'ns)
    (global-set-key (kbd "s-<up>") 'toggle-frame-maximized))

;; set the font size based on monitor size
(let* ((geometry (alist-get 'geometry (car (display-monitor-attributes-list))))
       (w (nth 2 geometry))
       (h (nth 3 geometry)))
                                        ;(message "Monitor size: %d x %d" w h))
  (set-face-attribute 'default nil
                      :font "Iosevka"
                      :height (cond
                               ((and (eq w 1512) (eq h 982)) 190) ; built in MBP display
                               ((and (eq w 3606) (eq h 2404)) 180) ; Framework display
                               (t 170)) ; fallback
                      :weight 'regular))
