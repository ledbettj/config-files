;;; soft-stone-theme.el --- Emacs 24 theme with a light background.
;; Author: Martin Haesler
;; URL: http://github.com/mswift42/soft-stone-theme
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Emacs theme with a light background.
;; Copyright (C) 2013 , Martin Haesler

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

(deftheme soft-stone)
(custom-theme-set-faces
  'soft-stone
        '(default ((t (:background "#eeeae0" :foreground "#000000"))))
        '(font-lock-builtin-face ((t (:foreground "#9e0045"))))
        '(region ((t (:background "#242424" :foreground "#faf4c6"))))
        '(highlight ((t (:foreground "#c1b8a5" :background "#191919"))))
	'(hl-line ((t (:background "#cfcabe"))))
	'(fringe ((t (:background "#e7e1d4" :foreground "#434343"))))
	'(cursor ((t (:background "#626262"))))
        '(show-paren-match-face ((t (:background "#f03f3f"))))
        '(isearch ((t (:bold t :foreground "#f03f3f" :background "#e2e2e5"))))
        '(mode-line ((t (:box (:line-width 1 :color nil :style released-button) :bold t :foreground "#343434" :background "#dbd2bf"))))
        '(mode-line-inactive ((t (:box (:line-width 1 :color nil :style pressed-button) :foreground "#808080" :background "#d7cdb8"))))
        '(mode-line-buffer-id ((t (:bold t :foreground "#121212" :background nil))))
	'(mode-line-highlight ((t (:background "#808080"))))
	'(vertical-border ((t (:foreground "#232323"))))
        '(minibuffer-prompt ((t (:bold t :foreground "#121212"))))
        '(default-italic ((t (:italic t))))
	'(font-lock-comment-face ((t (:foreground "#373737"))))
	'(font-lock-negation-char-face ((t (:foreground "#ff6523"))))
	'(font-lock-reference-face ((t (:foreground "#b7c2d7"))))
	'(font-lock-constant-face ((t (:foreground "#374014"))))
        '(font-lock-doc-face ((t (:foreground "#373737"))))
        '(font-lock-function-name-face ((t (:foreground "#340557" :bold t))))
        '(font-lock-keyword-face ((t (:bold t :foreground "#490026"))))
        '(font-lock-string-face ((t (:foreground "#253952"))))
        '(font-lock-type-face ((t (:foreground "#014500"))))
        '(font-lock-variable-name-face ((t (:foreground "#014500"))))
        '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff6523"))))
	'(link ((t (:foreground "#f03f3f" :underline t))))
	'(org-code ((t (:foreground "#191919"))))
	'(org-hide ((t (:foreground "#686868"))))
        '(org-level-1 ((t (:bold t :foreground "#121212" :height 1.1))))
        '(org-level-2 ((t (:bold nil :foreground "#232323"))))
        '(org-level-3 ((t (:bold t :foreground "#393939"))))
        '(org-level-4 ((t (:bold nil :foreground "#af4f4b"))))
        '(org-date ((t (:underline t :foreground "#4b004d") )))
        '(org-footnote  ((t (:underline t :foreground "#525150"))))
        '(org-link ((t (:underline t :foreground "#00025c" ))))
        '(org-special-keyword ((t (:foreground "#45511a"))))
        '(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
        '(org-block ((t (:foreground "#323232"))))
        '(org-quote ((t (:inherit org-block :slant italic))))
        '(org-verse ((t (:inherit org-block :slant italic))))
        '(org-todo ((t :foreground "#5c0020" :bold t)))
        '(org-done ((t (:bold t :foreground "#403d37"))))
        '(org-warning ((t (:underline t :foreground "#ff0000"))))
        '(org-agenda-structure ((t (:weight bold :foreground "#434343" :box (:color "#727272") :background "#aca49f"))))
        '(org-agenda-date ((t (:foreground "#00025c" :height 1.1 ))))
        '(org-agenda-date-weekend ((t (:weight normal :foreground "#787878"))))
        '(org-agenda-date-today ((t (:weight bold :foreground "#5c0020" :height 1.4))))
	'(org-scheduled ((t (:foreground "#4b004d"))))
	'(org-ellipsis ((t (:foreground "#5c0020"))))
	'(font-latex-bold-face ((t (:foreground "#cd8b00"))))
	'(font-latex-italic-face ((t (:foreground "#808bed" :italic t))))
	'(font-latex-string-face ((t (:foreground "#708090"))))
	'(font-latex-match-reference-keywords ((t (:foreground "#708090"))))
	'(font-latex-match-variable-keywords ((t (:foreground "#ebe6db"))))
	'(ido-only-match ((t (:foreground "#ff0000"))))
	'(org-sexp-date ((t (:foreground "#808080"))))
	'(ido-first-match ((t (:foreground "#494d56" :bold t))))
	'(gnus-header-content ((t (:foreground "#e3e3e3"))))
	'(gnus-header-from ((t (:foreground "#54686d"))))
	'(gnus-header-name ((t (:foreground "#5d90cd"))))
	'(gnus-header-subject ((t (:foreground "#b998df"))))
	'(magit-item-highlight ((t (:background "#cfcabe"))))
	'(mu4e-view-url-number-face ((t (:foreground "#af4f4b"))))
	'(mu4e-cited-1-face ((t (:foreground "#a2a1a0"))))
	'(mu4e-cited-7-face ((t (:foreground "#b2b1b0"))))
	'(mu4e-header-marks-face ((t (:foreground "#929190"))))
	'(ffap ((t (:foreground "#434241"))))
	'(js2-private-function-call ((t (:foreground "#45511a"))))
	'(js2-jsdoc-html-tag-delimiter ((t (:foreground "#454545"))))
	'(js2-jsdoc-html-tag-name ((t (:foreground "#525210"))))
	'(warning ((t (:foreground "#ff0000"))))
	'(powerline-active1 ((t (:foreground "#000000" :background "#d7cdb8"))))
	'(powerline-active2 ((t (:foreground "#000000" :background "#aca49f"))))
	'(powerline-inactive1 ((t (:foreground "#929292" :background "#d7cdb8"))))
	'(powerline-inactive2 ((t (:foreground "#929292" :background "#aca49f"))))
	'(ac-completion-face ((t (:underline t :foreground "#5c0020"))))
	'(slime-repl-inputed-output-face ((t (:foreground "#f03f3f")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'soft-stone)

;;; soft-stone-theme.el ends here
