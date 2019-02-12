;;; init.el --- youngker's configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.3.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; youngker's Emacs configuration
;;
;; See documentation on https://github.com/youngker/.emacs.d

;;; Code:

(defconst start-time (current-time))

;;; Basic preferences

(tooltip-mode -1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; sane defaults
(setq-default
 auto-revert-verbose nil
 blink-matching-paren nil
 column-number-mode t
 confirm-nonexistent-file-or-buffer nil
 delete-by-moving-to-trash nil
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 electric-indent-mode nil
 fill-column 80
 font-use-system-font t
 gc-cons-threshold 50000000
 global-auto-revert-non-file-buffers t
 history-length 1000
 indent-tabs-mode nil
 inhibit-startup-message t
 initial-scratch-message nil
 initial-major-mode 'text-mode
 large-file-warning-threshold 100000000
 make-backup-files nil
 next-error-highlight t
 next-error-highlight-no-select t
 query-replace-highlight t
 recenter-redisplay nil
 require-final-newline t
 ring-bell-function #'ignore
 scroll-conservatively 100000
 scroll-margin 0
 scroll-preserve-screen-position 1
 sentence-end-double-space nil
 shift-select-mode nil
 show-trailing-whitespace t
 tab-always-indent 'complete
 transient-mark-mode t
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell nil
 visible-cursor nil
 x-select-enable-clipboard t)

(setq default-frame-alist
      (append '((width . 80)
                (height . 40)
                (font . "Operator Mono SSm Medium:pixelsize=15"))
              default-frame-alist))

(auto-compression-mode +1)
(blink-cursor-mode -1)
(global-auto-revert-mode +1)
(global-font-lock-mode +1)
(global-hl-line-mode +1)
(savehist-mode +1)
(show-paren-mode +1)
(winner-mode +1)
(xterm-mouse-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; locale
(set-language-environment "Korean")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Bootstrap `use-package'

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize 'noactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(package-initialize)
;; (let ((default-directory (concat user-emacs-directory "elpa/")))
;;   (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-verbose t)
(setq use-package-always-ensure t)

;;; Themes
;; use true color for terminal

(let ((class '((class color) (min-colors 89)))
      (nord00 "#2E3440")
      (nord01 "#3B4252")
      (nord02 "#434C5E")
      (nord03 "#4C566A")
      (nord04 "#D8DEE9")
      (nord05 "#E5E9F0")
      (nord06 "#ECEFF4")
      (nord07 "#8FBCBB")
      (nord08 "#88C0D0")
      (nord09 "#81A1C1")
      (nord10 "#5E81AC")
      (nord11 "#BF616A")
      (nord12 "#D08770")
      (nord13 "#EBCB8B")
      (nord14 "#A3BE8C")
      (nord15 "#B48EAD"))
  (custom-theme-set-faces
   'user
   `(normal ((,class (:weight normal))))
   `(normal-italic ((,class (:weight normal :slant italic))))
   `(default ((,class (:foreground ,nord04 :background ,nord00))))
   `(error ((,class (:foreground ,nord11 :weight normal))))
   `(escape-glyph ((,class (:foreground ,nord12))))
   `(font-lock-builtin-face ((,class (:foreground ,nord09))))
   `(font-lock-comment-face ((,class (:foreground ,nord03 :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,nord03))))
   `(font-lock-constant-face ((,class (:foreground ,nord09))))
   `(font-lock-doc-face ((,class (:foreground ,nord03 :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,nord08))))
   `(font-lock-keyword-face ((,class (:foreground ,nord09))))
   `(font-lock-negation-char-face ((,class (:foreground ,nord09))))
   `(font-lock-preprocessor-face ((,class (:foreground ,nord10 :weight normal))))
   `(font-lock-reference-face ((,class (:foreground ,nord09))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,nord13))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,nord13))))
   `(font-lock-string-face ((,class (:foreground ,nord14))))
   `(font-lock-type-face ((,class (:foreground ,nord07))))
   `(font-lock-variable-name-face ((,class (:foreground ,nord04))))
   `(font-lock-warning-face ((,class (:foreground ,nord13))))
   `(italic ((,class (:slant italic))))
   `(shadow ((,class (:foreground ,nord03))))
   `(underline ((,class (:underline t))))
   `(warning ((,class (:foreground ,nord13 :weight normal))))
   `(c-annotation-face ((,class (:foreground ,nord12))))
   `(ediff-current-diff-A ((,class (:foreground ,nord06 :background ,nord11))))
   `(ediff-current-diff-Ancestor ((,class (:foreground ,nord06 :background ,nord11))))
   `(ediff-current-diff-B ((,class (:foreground ,nord06 :background ,nord12))))
   `(ediff-current-diff-C ((,class (:foreground ,nord06 :background ,nord10))))
   `(ediff-even-diff-A ((,class (:background ,nord01))))
   `(ediff-even-diff-Ancestor ((,class (:background ,nord01))))
   `(ediff-even-diff-B ((,class (:background ,nord01))))
   `(ediff-even-diff-C ((,class (:background ,nord01))))
   `(ediff-fine-diff-A ((,class (:foreground ,nord06 :background ,nord14 :weight normal))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,nord06 :background ,nord14 weight normal))))
   `(ediff-fine-diff-B ((,class (:foreground ,nord06 :background ,nord15 :weight normal))))
   `(ediff-fine-diff-C ((,class (:foreground ,nord06 :background ,nord13 :weight normal))))
   `(ediff-odd-diff-A ((,class (:background ,nord02))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,nord02))))
   `(ediff-odd-diff-B ((,class (:background ,nord02))))
   `(ediff-odd-diff-C ((,class (:background ,nord02))))
   `(diff-added ((,class (:foreground ,nord14))))
   `(diff-changed ((,class (:foreground ,nord13))))
   `(diff-context ((,class (:inherit default))))
   `(diff-file-header ((,class (:foreground ,nord08))))
   `(diff-function ((,class (:foreground ,nord07))))
   `(diff-header ((,class (:foreground ,nord09 :weight normal))))
   `(diff-hunk-header ((,class (:foreground ,nord09 :background ,nord00))))
   `(diff-indicator-added ((,class (:foreground ,nord14))))
   `(diff-indicator-changed ((,class (:foreground ,nord13))))
   `(diff-indicator-removed ((,class (:foreground ,nord11))))
   `(diff-nonexistent ((,class (:foreground ,nord11))))
   `(diff-refine-added ((,class (:foreground ,nord14))))
   `(diff-refine-changed ((,class (:foreground ,nord13))))
   `(diff-refine-removed ((,class (:foreground ,nord11))))
   `(diff-removed ((,class (:foreground ,nord11))))
   `(border ((,class (:foreground ,nord04))))
   `(buffer-menu-buffer ((,class (:foreground ,nord04 :weight normal))))
   `(button ((,class (:background ,nord00 :foreground ,nord08 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
   `(completions-annotations ((,class (:foreground ,nord09))))
   `(completions-common-part ((,class (:foreground ,nord08 :weight normal))))
   `(completions-first-difference ((,class (:foreground ,nord11))))
   `(custom-button ((,class (:background ,nord00 :foreground ,nord08 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
   `(custom-button-mouse ((,class (:background ,nord04 :foreground ,nord00 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
   `(custom-button-pressed ((,class (:background ,nord06 :foreground ,nord00 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
   `(custom-button-pressed-unraised ((,class (:background ,nord04 :foreground ,nord00 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
   `(custom-button-unraised ((,class (:background ,nord00 :foreground ,nord08 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
   `(custom-changed ((,class (:foreground ,nord13))))
   `(custom-comment ((,class (:foreground ,nord03))))
   `(custom-comment-tag ((,class (:foreground ,nord07))))
   `(custom-documentation ((,class (:foreground ,nord04))))
   `(custom-group-tag ((,class (:foreground ,nord08 :weight normal))))
   `(custom-group-tag-1 ((,class (:foreground ,nord08 :weight normal))))
   `(custom-invalid ((,class (:foreground ,nord11))))
   `(custom-modified ((,class (:foreground ,nord13))))
   `(custom-rogue ((,class (:foreground ,nord12 :background ,nord02))))
   `(custom-saved ((,class (:foreground ,nord14))))
   `(custom-set ((,class (:foreground ,nord08))))
   `(custom-state ((,class (:foreground ,nord14))))
   `(custom-themed ((,class (:foreground ,nord08 :background ,nord02))))
   `(cursor ((,class (:background ,nord04))))
   `(fringe ((,class (:foreground ,nord04 :background ,nord00))))
   `(file-name-shadow ((,class (:inherit shadow))))
   `(header-line ((,class (:foreground ,nord04 :background ,nord02))))
   `(help-argument-name ((,class (:foreground ,nord08))))
   `(highlight ((,class (:foreground ,nord08 :background ,nord02))))
   `(hl-line ((,class (:background ,nord01))))
   `(info-menu-star ((,class (:foreground ,nord09))))
   `(isearch ((,class (:foreground ,nord00 :background ,nord08))))
   `(isearch-fail ((,class (:foreground ,nord11))))
   `(link ((,class (:underline t))))
   `(link-visited ((,class (:underline t))))
   `(linum ((,class (:foreground ,nord03 :background ,nord00))))
   `(linum-relative-current-face ((,class (:foreground ,nord03 :background ,nord00))))
   `(match ((,class (:inherit isearch))))
   `(message-cited-text ((,class (:foreground ,nord04))))
   `(message-header-cc ((,class (:foreground ,nord09))))
   `(message-header-name ((,class (:foreground ,nord07))))
   `(message-header-newsgroup ((,class (:foreground ,nord14))))
   `(message-header-other ((,class (:foreground ,nord04))))
   `(message-header-subject ((,class (:foreground ,nord08))))
   `(message-header-to ((,class (:foreground ,nord09))))
   `(message-header-xheader ((,class (:foreground ,nord13))))
   `(message-mml ((,class (:foreground ,nord10))))
   `(message-separator ((,class (:inherit shadow))))
   `(minibuffer-prompt ((,class (:foreground ,nord08 :weight normal))))
   `(mm-command-output ((,class (:foreground ,nord08))))
   `(mode-line ((,class (:foreground ,nord08 :background ,nord03))))
   `(mode-line-buffer-id ((,class (:weight normal))))
   `(mode-line-highlight ((,class (:inherit highlight))))
   `(mode-line-inactive ((,class (:foreground ,nord04 :background ,nord03))))
   `(next-error ((,class (:inherit error))))
   `(nobreak-space ((,class (:foreground ,nord03))))
   `(outline-1 ((,class (:foreground ,nord08 :weight normal))))
   `(outline-2 ((,class (:inherit outline-1))))
   `(outline-3 ((,class (:inherit outline-1))))
   `(outline-4 ((,class (:inherit outline-1))))
   `(outline-5 ((,class (:inherit outline-1))))
   `(outline-6 ((,class (:inherit outline-1))))
   `(outline-7 ((,class (:inherit outline-1))))
   `(outline-8 ((,class (:inherit outline-1))))
   `(package-description ((,class (:foreground ,nord04))))
   `(package-help-section-name ((,class (:foreground ,nord08 :weight normal))))
   `(package-name ((,class (:foreground ,nord08))))
   `(package-status-available ((,class (:foreground ,nord07))))
   `(package-status-avail-obso ((,class (:foreground ,nord07 :slant italic))))
   `(package-status-built-in ((,class (:foreground ,nord09))))
   `(package-status-dependency ((,class (:foreground ,nord08 :slant italic))))
   `(package-status-disabled ((,class (:foreground ,nord03))))
   `(package-status-external ((,class (:foreground ,nord12 :slant italic))))
   `(package-status-held ((,class (:foreground ,nord04 :weight normal))))
   `(package-status-new ((,class (:foreground ,nord14))))
   `(package-status-incompat ((,class (:foreground ,nord11))))
   `(package-status-installed ((,class (:foreground ,nord07 :weight normal))))
   `(package-status-unsigned ((,class (:underline ,nord13))))
   `(query-replace ((,class (:foreground ,nord08 :background ,nord02))))
   `(region ((,class (:foreground ,nord00 :background ,nord08))))
   `(scroll-bar ((,class (:background ,nord03))))
   `(secondary-selection ((,class (:background ,nord02))))
   `(show-paren-match-face ((,class (:foreground ,nord00 :background ,nord08))))
   `(show-paren-mismatch-face ((,class (:background ,nord11))))
   `(success ((,class (:foreground ,nord14))))
   `(term ((,class (:foreground ,nord04 :background ,nord00))))
   `(term-color-black ((,class (:foreground ,nord01 :background ,nord01))))
   `(term-color-white ((,class (:foreground ,nord05 :background ,nord05))))
   `(term-color-cyan ((,class (:foreground ,nord07 :background ,nord07))))
   `(term-color-blue ((,class (:foreground ,nord08 :background ,nord08))))
   `(term-color-red ((,class (:foreground ,nord11 :background ,nord11))))
   `(term-color-yellow ((,class (:foreground ,nord13 :background ,nord13))))
   `(term-color-green ((,class (:foreground ,nord14 :background ,nord14))))
   `(term-color-magenta ((,class (:foreground ,nord15 :background ,nord15))))
   `(tool-bar ((,class (:foreground ,nord04 :background ,nord03))))
   `(tooltip ((,class (:foreground ,nord00 :background ,nord04))))
   `(trailing-whitespace ((,class (:background ,nord11))))
   `(tty-menu-disabled-face ((,class (:foreground ,nord01))))
   `(tty-menu-enabled-face ((,class (:background ,nord02 foreground ,nord04))))
   `(tty-menu-selected-face ((,class (:foreground ,nord08 :underline t))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,nord08))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,nord04))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,nord04))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,nord09))))
   `(vc-conflict-state ((,class (:foreground ,nord12))))
   `(vc-edited-state ((,class (:foreground ,nord13))))
   `(vc-locally-added-state ((,class (:underline ,nord14))))
   `(vc-locked-state ((,class (:foreground ,nord10))))
   `(vc-missing-state ((,class (:foreground ,nord11))))
   `(vc-needs-update-state ((,class (:foreground ,nord12))))
   `(vc-removed-state ((,class (:foreground ,nord11))))
   `(vc-state-base ((,class (:foreground ,nord04))))
   `(vc-up-to-date-state ((,class (:foreground ,nord08))))
   `(vertical-border ((,class (:foreground ,nord02))))
   `(which-func ((,class (:foreground ,nord08))))
   `(whitespace-big-indent ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-empty ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-hspace ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-indentation ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-line ((,class (:background ,nord00))))
   `(whitespace-newline ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-space ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-space-after-tab ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-space-before-tab ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-tab ((,class (:foreground ,nord03 :background ,nord00))))
   `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
   `(widget-button-pressed ((,class (:foreground ,nord09 :background ,nord01))))
   `(widget-documentation ((,class (:foreground ,nord04))))
   `(widget-field ((,class (:background ,nord02 :foreground ,nord04))))
   `(widget-single-line-field ((,class (:background ,nord02 :foreground ,nord04))))
   `(window-divider ((,class (:background ,nord03))))
   `(window-divider-first-pixel ((,class (:background ,nord03))))
   `(window-divider-last-pixel ((,class (:background ,nord03))))
   `(font-latex-normal-face ((,class (:inherit normal))))
   `(font-latex-italic-face ((,class (:inherit italic))))
   `(font-latex-math-face ((,class (:foreground ,nord08))))
   `(font-latex-sectioning-0-face ((,class (:foreground ,nord08 :weight normal))))
   `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-2-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-3-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-4-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-5-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-script-char-face ((,class (:inherit font-lock-warning-face))))
   `(font-latex-string-face ((,class (:inherit font-lock-string-face))))
   `(font-latex-warning-face ((,class (:inherit font-lock-warning-face))))
   `(elixir-attribute-face ((,class (:foreground ,nord12))))
   `(elixir-atom-face ((,class (:foreground ,nord04 :weight normal))))
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,nord14))))
   `(enh-ruby-op-face ((,class (:foreground ,nord09))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,nord13))))
   `(enh-ruby-regexp-face ((,class (:foreground ,nord13))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,nord14))))
   `(erm-syn-errline ((,class (:foreground ,nord11 :underline t))))
   `(erm-syn-warnline ((,class (:foreground ,nord13 :underline t))))
   `(jdee-db-active-breakpoint-face ((,class (:background ,nord02 :weight normal))))
   `(jdee-bug-breakpoint-cursor ((,class (:background ,nord02))))
   `(jdee-db-requested-breakpoint-face ((,class (:foreground ,nord13 :background ,nord02 :weight normal))))
   `(jdee-db-spec-breakpoint-face ((,class (:foreground ,nord14 :background ,nord02 :weight normal))))
   `(jdee-font-lock-api-face ((,class (:foreground ,nord04))))
   `(jdee-font-lock-code-face ((,class (:slant italic))))
   `(jdee-font-lock-constant-face ((,class (:foreground ,nord09))))
   `(jdee-font-lock-constructor-face ((,class (:foreground ,nord08))))
   `(jdee-font-lock-doc-tag-face ((,class (:foreground ,nord07))))
   `(jdee-font-lock-link-face ((,class (:underline t))))
   `(jdee-font-lock-modifier-face ((,class (:foreground ,nord09))))
   `(jdee-font-lock-number-face ((,class (:foreground ,nord15))))
   `(jdee-font-lock-operator-fac ((,class (:foreground ,nord09))))
   `(jdee-font-lock-package-face ((,class (:foreground ,nord07))))
   `(jdee-font-lock-pre-face ((,class (:foreground ,nord03 :slant italic))))
   `(jdee-font-lock-private-face ((,class (:foreground ,nord09))))
   `(jdee-font-lock-public-face ((,class (:foreground ,nord09))))
   `(jdee-font-lock-variable-face ((,class (:foreground ,nord04))))
   `(js2-function-call ((,class (:foreground ,nord08))))
   `(js2-private-function-call ((,class (:foreground ,nord08))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,nord06))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,nord09))))
   `(js2-external-variable ((,class (:foreground ,nord04))))
   `(js2-function-param ((,class (:foreground ,nord04))))
   `(js2-jsdoc-value ((,class (:foreground ,nord03))))
   `(js2-jsdoc-tag ((,class (:foreground ,nord07))))
   `(js2-jsdoc-type ((,class (:foreground ,nord07))))
   `(js2-private-member ((,class (:foreground ,nord04))))
   `(js2-object-property ((,class (:foreground ,nord04))))
   `(js2-error ((,class (:foreground ,nord11))))
   `(js2-warning ((,class (:foreground ,nord13))))
   `(js2-instance-member ((,class (:foreground ,nord04))))
   `(js3-error-face ((,class (:foreground ,nord11))))
   `(js3-external-variable-face ((,class (:foreground ,nord04))))
   `(js3-function-param-face ((,class (:foreground ,nord04))))
   `(js3-instance-member-face ((,class (:foreground ,nord04))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,nord06))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,nord09))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,nord09))))
   `(js3-jsdoc-type-face ((,class (:foreground ,nord07))))
   `(js3-jsdoc-value-face ((,class (:foreground ,nord04))))
   `(js3-magic-paren-face ((,class (:inherit show-paren-match-face))))
   `(js3-private-function-call-face ((,class (:foreground ,nord08))))
   `(js3-private-member-face ((,class (:foreground ,nord04))))
   `(js3-warning-face ((,class (:foreground ,nord13))))
   `(markdown-blockquote-face ((,class (:foreground ,nord03))))
   `(markdown-normal-face ((,class (:inherit normal))))
   `(markdown-header-face-1 ((,class (:foreground ,nord08))))
   `(markdown-header-face-2 ((,class (:foreground ,nord08))))
   `(markdown-header-face-3 ((,class (:foreground ,nord08))))
   `(markdown-header-face-4 ((,class (:foreground ,nord08))))
   `(markdown-header-face-5 ((,class (:foreground ,nord08))))
   `(markdown-header-face-6 ((,class (:foreground ,nord08))))
   `(markdown-inline-code-face ((,class (:foreground ,nord07))))
   `(markdown-italic-face ((,class (:inherit italic))))
   `(markdown-link-face ((,class (:foreground ,nord08))))
   `(markdown-markup-face ((,class (:foreground ,nord09))))
   `(markdown-reference-face ((,class (:inherit markdown-link-face))))
   `(markdown-url-face ((,class (:foreground ,nord04 :underline t))))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,nord07)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,nord08)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,nord09)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,nord10)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,nord12)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,nord13)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,nord14)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,nord15)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,nord11)))
   `(web-mode-attr-tag-custom-face ((,class (:foreground ,nord07))))
   `(web-mode-builtin-face ((,class (:foreground ,nord09))))
   `(web-mode-comment-face ((,class (:foreground ,nord03))))
   `(web-mode-comment-keyword-face ((,class (:foreground ,nord03))))
   `(web-mode-constant-face ((,class (:foreground ,nord04))))
   `(web-mode-css-at-rule-face ((,class (:foreground ,nord12))))
   `(web-mode-css-function-face ((,class (:foreground ,nord08))))
   `(web-mode-css-property-name-face ((,class (:foreground ,nord09))))
   `(web-mode-css-pseudo-class-face ((,class (:foreground ,nord07))))
   `(web-mode-css-selector-face ((,class (:foreground ,nord09))))
   `(web-mode-css-string-face ((,class (:foreground ,nord14))))
   `(web-mode-doctype-face ((,class (:foreground ,nord10))))
   `(web-mode-function-call-face ((,class (:foreground ,nord08))))
   `(web-mode-function-name-face ((,class (:foreground ,nord08))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,nord07))))
   `(web-mode-html-attr-equal-face ((,class (:foreground ,nord04))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,nord14))))
   `(web-mode-html-entity-face ((,class (:foreground ,nord09))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,nord04))))
   `(web-mode-html-tag-custom-face ((,class (:foreground ,nord09))))
   `(web-mode-html-tag-face ((,class (:foreground ,nord09))))
   `(web-mode-html-tag-namespaced-face ((,class (:foreground ,nord09))))
   `(web-mode-json-key-face ((,class (:foreground ,nord07))))
   `(web-mode-json-string-face ((,class (:foreground ,nord14))))
   `(web-mode-keyword-face ((,class (:foreground ,nord09))))
   `(web-mode-preprocessor-face ((,class (:foreground ,nord10))))
   `(web-mode-string-face ((,class (:foreground ,nord14))))
   `(web-mode-symbol-face ((,class (:foreground ,nord04))))
   `(web-mode-type-face ((,class (:foreground ,nord07))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-variable-name-face ((,class (:foreground ,nord04))))
   `(anzu-mode-line ((,class (:foreground, nord08))))
   `(anzu-mode-line-no-match ((,class (:foreground, nord11))))
   `(avy-lead-face ((,class (:background ,nord11 :foreground ,nord05))))
   `(avy-lead-face-0 ((,class (:background ,nord10 :foreground ,nord05))))
   `(avy-lead-face-1 ((,class (:background ,nord03 :foreground ,nord05))))
   `(avy-lead-face-2 ((,class (:background ,nord15 :foreground ,nord05))))
   `(company-echo-common ((,class (:foreground ,nord00 :background ,nord04))))
   `(company-preview ((,class (:foreground ,nord04 :background ,nord10))))
   `(company-preview-common ((,class (:foreground ,nord00 :background ,nord08))))
   `(company-preview-search ((,class (:foreground ,nord00 :background ,nord08))))
   `(company-scrollbar-bg ((,class (:foreground ,nord01 :background ,nord01))))
   `(company-scrollbar-fg ((,class (:foreground ,nord02 :background ,nord02))))
   `(company-template-field ((,class (:foreground ,nord00 :background ,nord07))))
   `(company-tooltip ((,class (:foreground ,nord04 :background ,nord02))))
   `(company-tooltip-annotation ((,class (:foreground ,nord12))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,nord12 :weight normal))))
   `(company-tooltip-common ((,class (:foreground ,nord08))))
   `(company-tooltip-common-selection ((,class (:foreground ,nord08 :background ,nord03))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,nord03 :weight normal))))
   `(diff-hl-change ((,class (:background ,nord13))))
   `(diff-hl-insert ((,class (:background ,nord14))))
   `(diff-hl-delete ((,class (:background ,nord11))))
   `(evil-ex-info ((,class (:foreground ,nord08))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,nord09))))
   `(evil-ex-substitute-matches ((,class (:inherit isearch))))
   `(flycheck-error ((,class (:underline (:style wave :color ,nord11)))))
   `(flycheck-fringe-error ((,class (:foreground ,nord11 :weight normal))))
   `(flycheck-fringe-info ((,class (:foreground ,nord08 :weight normal))))
   `(flycheck-fringe-warning ((,class (:foreground ,nord13 :weight normal))))
   `(flycheck-info ((,class (:underline (:style wave :color ,nord08)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,nord13)))))
   `(git-gutter:modified ((,class (:foreground ,nord13))))
   `(git-gutter:added ((,class (:foreground ,nord14))))
   `(git-gutter:deleted ((,class (:foreground ,nord11))))
   `(git-gutter+-modified ((,class (:foreground ,nord13))))
   `(git-gutter+-added ((,class (:foreground ,nord14))))
   `(git-gutter+-deleted ((,class (:foreground ,nord11))))
   `(helm-bookmark-addressbook ((,class (:foreground ,nord07))))
   `(helm-bookmark-directory ((,class (:foreground ,nord09))))
   `(helm-bookmark-file ((,class (:foreground ,nord08))))
   `(helm-bookmark-gnus ((,class (:foreground ,nord10))))
   `(helm-bookmark-info ((,class (:foreground ,nord14))))
   `(helm-bookmark-man ((,class (:foreground ,nord04))))
   `(helm-bookmark-w3m ((,class (:foreground ,nord09))))
   `(helm-buffer-directory ((,class (:foreground ,nord09))))
   `(helm-buffer-file ((,class (:foreground ,nord08))))
   `(helm-buffer-not-saved ((,class (:foreground ,nord13))))
   `(helm-buffer-process ((,class (:foreground ,nord10))))
   `(helm-candidate-number ((,class (:foreground ,nord13 :weight normal))))
   `(helm-candidate-number-suspended ((,class (:foreground ,nord04))))
   `(helm-ff-directory ((,class (:foreground ,nord09 :weight normal))))
   `(helm-ff-dirs ((,class (:foreground ,nord09))))
   `(helm-ff-dotted-directory ((,class (:foreground ,nord09))))
   `(helm-ff-dotted-symlink-directory ((,class (:foreground ,nord07 :weight normal))))
   `(helm-ff-executable ((,class (:foreground ,nord08))))
   `(helm-ff-file ((,class (:foreground ,nord04))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,nord11 :weight normal))))
   `(helm-ff-prefix ((,class (:foreground ,nord00 :background ,nord09))))
   `(helm-ff-symlink ((,class (:foreground ,nord07))))
   `(helm-grep-cmd-line ((,class (:foreground ,nord04 :background ,nord00))))
   `(helm-grep-file ((,class (:foreground ,nord08))))
   `(helm-grep-finish ((,class (:foreground ,nord05))))
   `(helm-grep-lineno ((,class (:foreground ,nord04))))
   `(helm-grep-match ((,class (:inherit isearch))))
   `(helm-grep-running ((,class (:foreground ,nord08))))
   `(helm-header ((,class (:foreground ,nord09 :background ,nord02))))
   `(helm-header-line-left-margin ((,class (:foreground ,nord09 :background ,nord02))))
   `(helm-history-deleted ((,class (:foreground ,nord11))))
   `(helm-history-remote ((,class (:foreground ,nord04))))
   `(helm-lisp-completion-info ((,class (:foreground ,nord04 :weight normal))))
   `(helm-lisp-show-completion ((,class (:inherit isearch))))
   `(helm-locate-finish ((,class (:foreground ,nord14))))
   `(helm-match ((,class (:foreground ,nord08))))
   `(helm-match-item ((,class (:inherit isearch))))
   `(helm-moccur-buffer ((,class (:foreground ,nord08))))
   `(helm-resume-need-update ((,class (:foreground ,nord00 :background ,nord13))))
   `(helm-selection ((,class (:inherit highlight))))
   `(helm-selection-line ((,class (:background ,nord02))))
   `(helm-source-header ((,class (:height 1.44 :foreground ,nord08 :background ,nord02))))
   `(helm-separator ((,class (:background ,nord02))))
   `(helm-visible-mark ((,class (:background ,nord02))))
   `(helm-codesearch-file-face ((,class (:foreground ,nord14 :slant italic))))
   `(helm-codesearch-lineno-face ((,class (:foreground ,nord13))))
   `(helm-codesearch-source-face ((,class (:foreground ,nord03))))
   `(magit-branch ((,class (:foreground ,nord07 :weight normal))))
   `(magit-diff-context-highlight ((,class (:background ,nord02))))
   `(magit-diff-file-header ((,class (:foreground ,nord08 :box (:color ,nord08)))))
   `(magit-diffstat-added ((,class (:foreground ,nord14))))
   `(magit-diffstat-removed ((,class (:foreground ,nord11))))
   `(magit-hash ((,class (:foreground ,nord08))))
   `(magit-hunk-heading ((,class (:foreground ,nord09))))
   `(magit-hunk-heading-highlight ((,class (:foreground ,nord09 :background ,nord02))))
   `(magit-item-highlight ((,class (:foreground ,nord08 :background ,nord02))))
   `(magit-log-author ((,class (:foreground ,nord07))))
   `(magit-process-ng ((,class (:foreground ,nord13 :weight normal))))
   `(magit-process-ok ((,class (:foreground ,nord14 :weight normal))))
   `(magit-section-heading ((,class (:foreground ,nord07 :weight normal))))
   `(magit-section-highlight ((,class (:background ,nord02))))
   `(magit-diff-added ((,class (:foreground ,nord14))))
   `(magit-diff-added-highlight ((,class (:foreground ,nord14 :background ,nord02))))
   `(magit-diff-removed ((,class (:foreground ,nord11))))
   `(magit-diff-removed-highlight ((,class (:foreground ,nord11 :background ,nord02))))
   `(mu4e-header-marks-face ((,class (:foreground ,nord09))))
   `(mu4e-title-face ((,class (:foreground ,nord08))))
   `(mu4e-header-key-face ((,class (:foreground ,nord08))))
   `(mu4e-highlight-face ((,class (:highlight))))
   `(mu4e-flagged-face ((,class (:foreground ,nord13))))
   `(mu4e-unread-face ((,class (:foreground ,nord13 :weight normal))))
   `(mu4e-link-face ((,class (:underline t))))
   `(powerline-active1 ((,class (:foreground ,nord04 :background ,nord01))))
   `(powerline-active2 ((,class (:foreground ,nord04 :background ,nord03))))
   `(powerline-inactive1 ((,class (:background ,nord02))))
   `(powerline-inactive2 ((,class (:background ,nord02))))
   `(powerline-evil-base-face ((,class (:foreground ,nord04))))
   `(powerline-evil-normal-face ((,class (:background ,nord08))))
   `(powerline-evil-insert-face ((,class (:foreground ,nord00 :background ,nord04))))
   `(powerline-evil-visual-face ((,class (:foreground ,nord00 :background ,nord07))))
   `(powerline-evil-replace-face ((,class (:foreground ,nord00 :background ,nord09))))
   `(neo-banner-face ((,class (:foreground ,nord10))))
   `(neo-dir-link-face ((,class (:foreground ,nord09))))
   `(neo-expand-btn-face ((,class (:foreground ,nord06 :normal t))))
   `(neo-file-link-face ((,class (:foreground ,nord04))))
   `(neo-root-dir-face ((,class (:foreground ,nord07 :weight normal))))
   `(neo-vc-added-face ((,class (:foreground ,nord14))))
   `(neo-vc-conflict-face ((,class (:foreground ,nord11))))
   `(neo-vc-default-face ((,class (:foreground ,nord04))))
   `(neo-vc-edited-face ((,class (:foreground ,nord13))))
   `(neo-vc-ignored-face ((,class (:foreground ,nord03))))
   `(neo-vc-missing-face ((,class (:foreground ,nord12))))
   `(neo-vc-needs-merge-face ((,class (:background ,nord12 :foreground ,nord04))))
   `(neo-vc-needs-update-face ((,class (:background ,nord10 :foreground ,nord04))))
   `(neo-vc-removed-face ((,class (:foreground ,nord11 :strike-through nil))))
   `(neo-vc-up-to-date-face ((,class (:foreground ,nord04))))
   `(neo-vc-user-face ((,class (:foreground ,nord04))))
   `(highlight-symbol-face ((,class (:foreground ,nord13 :background ,nord01))))
   `(ivy-current-match ((,class (:background ,nord10 :foreground ,nord04))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,nord10))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,nord10))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,nord10))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,nord10))))
   `(ivy-confirm-face ((,class (:foreground ,nord10))))
   `(ivy-match-required-face ((,class (:foreground ,nord10))))
   `(ivy-virtual ((,class (:foreground ,nord10))))
   `(ivy-action ((,class (:foreground ,nord10))))
   `(org-level-1 ((,class (:foreground ,nord08 :weight normal))))
   `(org-level-2 ((,class (:inherit org-level-1))))
   `(org-level-3 ((,class (:inherit org-level-1))))
   `(org-level-4 ((,class (:inherit org-level-1))))
   `(org-level-5 ((,class (:inherit org-level-1))))
   `(org-level-6 ((,class (:inherit org-level-1))))
   `(org-level-7 ((,class (:inherit org-level-1))))
   `(org-level-8 ((,class (:inherit org-level-1))))
   `(org-agenda-structure ((,class (:foreground ,nord09))))
   `(org-agenda-date ((,class (:foreground ,nord08 :underline nil))))
   `(org-agenda-done ((,class (:foreground ,nord14))))
   `(org-agenda-dimmed-todo-face ((,class (:background ,nord13))))
   `(org-block ((,class (:foreground ,nord04))))
   `(org-block-background ((,class (:background ,nord00))))
   `(org-block-begin-line ((,class (:foreground ,nord07))))
   `(org-block-end-line ((,class (:foreground ,nord07))))
   `(org-checkbox ((,class (:foreground ,nord09))))
   `(org-checkbox-statistics-done ((,class (:foreground ,nord14))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,nord13))))
   `(org-code ((,class (:foreground ,nord07))))
   `(org-column ((,class (:background ,nord02))))
   `(org-column-title ((,class (:inherit org-column :weight normal :underline t))))
   `(org-date ((,class (:foreground ,nord08))))
   `(org-document-info ((,class (:foreground ,nord04))))
   `(org-document-info-keyword ((,class (:foreground ,nord03 :weight normal))))
   `(org-document-title ((,class (:foreground ,nord08 :weight normal))))
   `(org-done ((,class (:foreground ,nord14 :weight normal))))
   `(org-ellipsis ((,class (:foreground ,nord03))))
   `(org-footnote ((,class (:foreground ,nord08))))
   `(org-formula ((,class (:foreground ,nord09))))
   `(org-hide ((,class (:foreground ,nord00 :background ,nord00))))
   `(org-link ((,class (:underline t))))
   `(org-scheduled ((,class (:foreground ,nord14))))
   `(org-scheduled-previously ((,class (:foreground ,nord13))))
   `(org-scheduled-today ((,class (:foreground ,nord08))))
   `(org-special-keyword ((,class (:foreground ,nord09))))
   `(org-table ((,class (:foreground ,nord09))))
   `(org-todo ((,class (:foreground ,nord13 :weight normal))))
   `(org-upcoming-deadline ((,class (:foreground ,nord12))))
   `(org-warning ((,class (:foreground ,nord13 :weight normal))))
   `(font-latex-normal-face ((,class (:inherit normal))))
   `(font-latex-italic-face ((,class (:slant italic))))
   `(font-latex-string-face ((,class (:foreground ,nord14))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,nord09))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,nord04))))
   `(ido-only-match ((,class (:foreground ,nord08))))
   `(org-sexp-date ((,class (:foreground ,nord07))))
   `(ido-first-match ((,class (:foreground ,nord08 :weight normal))))
   `(ido-subdir ((,class (:foreground ,nord09))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-agenda-date-weekend ((,class (:foreground ,nord09))))
   `(org-agenda-date-today ((,class (:foreground ,nord08 :weight normal))))
   `(org-agenda-done ((,class (:foreground ,nord14))))
   `(org-verbatim ((,class (:foreground ,nord07))))))

;;;

(use-package term
  :ensure nil
  :preface
  ;; from xterm-frobs.el
  (defvar xterm-screen-dcs-encapsulation
    (not (null (or (getenv "STY")
                   (save-match-data
                     (string-match "^screen\\(\\|-.*\\)$" (getenv "TERM")))))))

  (defun xterm-send-escape-sequence (string)
    (cond ((and xterm-screen-dcs-encapsulation
                (save-match-data (string-match "\e[P\\\\]" string)))
           (save-match-data
             (let ((pos 0)
                   (substrings nil))
               (while (string-match "\e\\(P\\|\\\\\\)" string pos)
                 (setq substrings
                       (cons "\e\\"
                             (cons (substring string pos (match-beginning 1))
                                   (cons "\eP" substrings))))
                 (setq pos (match-beginning 1)))
               (setq substrings (cons (substring string pos) substrings))
               (setq string (mapconcat 'identity (nreverse substrings) "")))))
          (xterm-screen-dcs-encapsulation
           (setq string (format "\eP%s\e\\" string))))
    (send-string-to-terminal string))

  (defun terminal-title-hook ()
    (if (not window-system)
        (xterm-send-escape-sequence
         (format "\e]2;%s\a"
                 (concat (system-name) ": "
                         (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name))
                           (buffer-name)))))))
  :init
  (setq frame-title-format
        '((:eval (system-name)) ": "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))
  (add-hook 'post-command-hook #'terminal-title-hook))

(use-package diminish
  :commands diminish)

(use-package server
  :ensure nil
  :commands (server-running-p
             server-start)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package auto-compile
  :commands (auto-compile-on-load-mode
             auto-compile-on-save-mode)
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands (global-page-break-lines-mode
             page-break-lines-mode)
  :config
  (global-page-break-lines-mode)
  (page-break-lines-mode))

(use-package whitespace-cleanup-mode
  :commands global-whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(use-package smooth-scrolling
  :disabled t)

(use-package subword
  :diminish subword-mode
  :commands (subword-mode
             global-subword-mode)
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :config
  (global-subword-mode +1))

(use-package recentf
  :commands recentf-mode
  :config
  (recentf-mode +1)
  (setq-default
   recentf-max-saved-items 100
   recentf-save-file (concat user-emacs-directory "recentf")))

(use-package saveplace
  :commands save-place-mode
  :config
  (setq-default
   ;;save-place t
   save-place-file (concat user-emacs-directory "place")))

(use-package uniquify
  :ensure nil
  :defer t
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;;; macos

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :defines (exec-path-from-shell-check-startup-files
            mac-option-modifier
            mac-command-modifier
            ns-function-modifier)
  :commands exec-path-from-shell-initialize
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))

(defun my-setup-hook ()
  "My setup hook."
  (auto-compile-on-load-mode)
  ;;(global-auto-complete-mode)
  (global-company-mode)
  (global-diff-hl-mode)
  (global-page-break-lines-mode)
  (global-whitespace-cleanup-mode)
  (shackle-mode)
  (recentf-mode)
  (save-place-mode +1)
  (server-running-p)
  (volatile-highlights-mode)
  (which-key-mode)
  (yas-global-mode))

;;; ido

(use-package ido
  :disabled t
  :bind
  (("C-x C-f" . ido-find-file)
   ("C-x b"   . ido-switch-buffer)
   ("C-x B"   . ido-switch-buffer-other-window))
  :commands (ido-fallback-command
             ido-complete
             ido-everywhere
             ido-select-text
             ido-exit-minibuffer)
  :init
  (add-hook 'ido-setup-hook #'my-setup-hook)
  :config
  (ido-mode +1)
  (ido-everywhere +1)
  (icomplete-mode +1)
  (setq ido-auto-merge-work-directories-length -1
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-use-virtual-buffers t
        ido-use-faces nil
        ido-use-filename-at-point nil)
  (define-key ido-file-completion-map (kbd "C-\\") 'backward-kill-word)

  (use-package flx-ido
    :demand t
    :commands (flx-ido-mode flx-ido-reset flx-ido-debug flx-ido-match)
    :config
    (flx-ido-mode +1))

  (use-package ido-vertical-mode
    :disabled t
    :config
    (ido-vertical-mode)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

  (use-package ido-complete-space-or-hyphen
    :config
    (ido-complete-space-or-hyphen-mode +1))

  (use-package ido-completing-read+
    :config
    (ido-ubiquitous-mode +1))

  (use-package ido-yes-or-no
    :config
    (ido-yes-or-no-mode +1))

  (use-package crm-custom
    :config
    (crm-custom-mode +1)))

;;; ivy

(use-package ivy
  :disabled t
  :diminish ivy-mode
  :commands ivy-mode
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-c C-r" . ivy-resume))
  :init
  (add-hook 'ivy-mode-hook #'my-setup-hook)
  :config
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-arrow))

(use-package counsel
  :disabled t
  :bind
  (("C-x C-f" . counsel-find-file)
   ("C-x C-i" . counsel-imenu)
   ("C-x f" . counsel-recentf)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop))
  :init
  (add-hook 'imenu-after-jump-hook #'recenter-top-bottom))

(use-package swiper
  :disabled t
  :functions (ivy-yank-word
              ivy-previous-line-or-history)
  :bind
  ("C-s" . swiper)
  :config
  (bind-keys :map ivy-minibuffer-map
    ("C-w" . ivy-yank-word)
    ("C-r" . ivy-previous-line-or-history)))

;;; helm

(use-package helm
  :defines (helm-idle-delay helm-quick-update)
  :bind
  (("C-x C-i"   . helm-imenu)
   ("C-c h o"   . helm-occur)
   ("C-c h a"   . helm-apropos)
   ("C-c h m"   . helm-man-woman)
   ("C-c h r"   . helm-regex)
   ("C-c h SPC" . helm-all-mark-rings)
   ("C-x C-f"   . helm-find-files)
   ("C-x f"     . helm-recentf)
   ("C-x b"     . helm-mini)
   ("C-x r b"   . helm-filtered-bookmarks)
   ("C-c h w"   . helm-descbinds)
   ("C-c h f"   . helm-codesearch-find-file)
   ("C-c h t"   . helm-codesearch-find-pattern)
   ("C-c h I"   . helm-codesearch-create-csearchindex)
   ("C-c h b"   . helm-resume)
   ("M-x"       . helm-M-x)
   ("M-y"       . helm-show-kill-ring))
  :init
  (add-hook 'helm-after-initialize-hook #'my-setup-hook)
  :config
  (use-package helm-command :ensure nil)
  (use-package helm-semantic :ensure nil)
  (use-package helm-ring :ensure nil)
  (use-package helm-config
    :ensure nil
    :demand t
    :commands (async-bytecomp-get-allowed-pkgs
               async-byte-recompile-directory))
  (use-package helm-descbinds)
  (use-package helm-codesearch)
  (cl-defmethod helm-setup-user-source ((source helm-source-multi-occur))
    (setf (slot-value source 'follow) 1))
  (setq helm-M-x-fuzzy-match        t
        helm-M-x-requires-pattern   nil
        helm-buffers-fuzzy-matching t
        helm-display-function       'pop-to-buffer
        helm-ff--auto-update-state  t
        helm-ff-auto-update-initial-value t
        helm-ff-skip-boring-files   t
        helm-idle-delay             0.1
        helm-imenu-fuzzy-match      t
        helm-input-idle-delay       0.1
        helm-quick-update           t
        helm-semantic-fuzzy-match   t
        helm-apropos-fuzzy-match    t
        helm-lisp-fuzzy-completion  t)
  (bind-keys :map isearch-mode-map
    ("C-o" . helm-occur-from-isearch)
    ("M-o" . helm-multi-occur-from-isearch)))

;;; tool

(use-package smex
  :disabled t
  :commands smex-initialize
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook `(lambda () (auto-revert-mode +1))))

(use-package avy
  :bind
  ("C-c ;" . avy-goto-char)
  :commands avy-setup-default
  :config
  (avy-setup-default))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package highlight-tail
  :disabled t
  :diminish highlight-tail-mode
  :config
  (setq highlight-tail-steps 16)
  (highlight-tail-mode))

(use-package win-switch
  :disabled t
  :bind
  ("C-x o" . win-switch-dispatch)
  :commands win-switch-set-keys
  :config
  (setq win-switch-feedback-background-color "#6c96af")
  (setq win-switch-feedback-foreground-color "#000000")
  (setq win-switch-window-threshold 1)
  (setq win-switch-idle-time 0.7)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window))

(use-package popwin
  :disabled t
  :commands popwin-mode
  :config
  (popwin-mode +1)
  (setq popwin:special-display-config
        '(("*Help*"
           :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*compilation*"
           :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Shell Command Output*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*Async Shell Command*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*undo-tree*"
           :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*ert*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*grep*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("^\*WoMan.+\*$"
           :regexp t :position bottom))))

(use-package shackle
  :commands shackle-mode
  :config
  (shackle-mode +1)
  (setq shackle-rules
        '(("*Help*" :align t :select t)
          ("*Google Translate*" :align t :select t)
          (" *undo-tree*" :align right :size 0.1)
          ((grep-mode compilation-mode) :align t :select t)
          ("\\`\\*helm.*?\\*\\'" :popup t :regexp t :align t)
          ("\\`\\*cider-repl .*" :regexp t :align t :size 0.2)
          ((inferior-scheme-mode "*shell*" "*eshell*") :popup t :align t))
        shackle-default-rule '(:select t)
        shackle-default-size 0.4
        shackle-inhibit-window-quit-on-same-windows t))

(use-package undo-tree
  :bind
  ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package ztree
  :commands ztree-diff
  :config
  (set-face-attribute
   'ztreep-diff-model-add-face  nil :foreground "#87cefa")
  (setq ztree-draw-unicode-lines t)
  (bind-keys :map ztreediff-mode-map
    ("p" . previous-line)
    ("n" . next-line)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-global-mode
  :bind
  ("C-M-y" . company-yasnippet)
  :config
  (use-package yasnippet-snippets)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (yas-global-mode +1))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :diminish aggressive-indent-mode
  :config
  (aggressive-indent-mode t))

(use-package expand-region
  :bind
  ("C-c =" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package visual-regexp
  :bind
  ("M-/" . vr/replace))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands
  (highlight-symbol-mode highlight-symbol-nav-mode)
  :bind
  ("C-c c" . highlight-symbol)
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook #'highlight-symbol-mode)
    (add-hook hook #'highlight-symbol-nav-mode))
  :config
  (setq highlight-symbol-colors
        '("#BF616A" "#D08770" "#EBCB8B" "#A3BE8C" "#B48EAD"
          "#5E81AC" "#81A1C1" "#88C0D0" "#8FBCBB"))
  (setq highlight-symbol-foreground-color "#D8DEE9"))

(use-package diff-hl
  :commands global-diff-hl-mode
  :config
  (global-diff-hl-mode +1)
  (diff-hl-margin-mode)
  (setq diff-hl-side 'right))

(use-package eopengrok
  :disabled t
  :bind
  (("C-c s i" . eopengrok-create-index)
   ("C-c s I" . eopengrok-create-index-with-enable-projects)
   ("C-c s d" . eopengrok-find-definition)
   ("C-c s f" . eopengrok-find-file)
   ("C-c s s" . eopengrok-find-reference)
   ("C-c s t" . eopengrok-find-text)
   ("C-c s h" . eopengrok-find-history)
   ("C-c s c" . eopengrok-find-custom)
   ("C-c s b" . eopengrok-resume)))

(use-package google-translate
  :defines google-translate-translation-directions-alist
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :init
  ;; (setq google-translate-output-destination 'popup)
  (setq google-translate-translation-directions-alist
        '(("en" . "ko") ("ko" . "en"))))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :preface
  (defun enable-rainbow-mode ()
    (when (string-match "\\(color-theme-\\|-theme\\|init\\.el\\)" (buffer-name))
      (rainbow-mode +1)))
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook #'rainbow-mode)))

(use-package projectile
  :diminish projectile-mode
  :defer t
  :config
  (projectile-mode))

(use-package helm-projectile
  :bind
  (("C-c p h" . helm-projectile)
   ("C-c p f" . helm-projectile-find-file)
   ("C-c p t" . helm-projectile-rg))
  :config
  (helm-projectile-on))

(use-package counsel-projectile
  :disabled t
  :bind
  (("C-c p p" . counsel-projectile-switch-project)
   ("C-c p b" . counsel-projectile-switch-to-buffer)
   ("C-c p f" . counsel-projectile-find-file)))

(use-package dired
  :ensure nil
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package flycheck
  :commands flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon
    'flycheck-show-error-at-point))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config
  (use-package color :ensure nil
    :commands color-saturate-name
    :demand t
    :config
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 20)))))

(use-package paredit-everywhere
  :diminish paredit-everywhere-mode
  :commands paredit-everywhere-mode
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
  (add-hook 'css-mode-hook #'paredit-everywhere-mode))

(use-package paredit
  :diminish paredit-mode
  :commands paredit-mode
  :config
  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))

(use-package company
  :diminish company-mode
  :defines company-dabbrev-downcase
  :commands (global-company-mode
             company-select-previous
             company-select-next
             company-complete-common-or-cycle)
  :init
  :config
  (setq company-dabbrev-downcase nil
        company-echo-delay 0
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 10
        company-transformers '(company-sort-by-occurrence))
  (global-company-mode +1)
  (bind-keys :map company-active-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)
    ("TAB" . company-complete-common-or-cycle)))

(use-package auto-complete
  :disabled t
  :diminish auto-complete-mode
  :commands global-auto-complete-mode
  :functions ac-flyspell-workaround
  :init
  (dolist (hook '(prog-mode-hook org-mode-hook markdown-mode-hook))
    (add-hook hook #'auto-complete-mode))
  :config
  (use-package auto-complete-config
    :ensure nil
    :demand t)
  (setq ac-auto-start 2
        ac-candidate-menu-min 0
        ac-disable-inline t
        ac-dwim t
        ac-quick-help-delay 1
        ac-quick-help-height 60
        ac-show-menu-immediately-on-auto-complete t
        ac-use-menu-map t
        ac-auto-show-menu t)
  (set-default 'ac-sources
               '(ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-semantic
                 ac-source-yasnippet))
  (ac-config-default)
  (ac-flyspell-workaround)
  (global-auto-complete-mode t)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "dict")))

(use-package eval-sexp-fu
  :disabled t
  :commands turn-on-eval-sexp-fu-flash-mode
  :config
  (use-package cider-eval-sexp-fu
    :demand t
    :commands cider-esf--bounds-of-last-sexp)
  (setq eval-sexp-fu-flash-duration 0.5))

(use-package mic-paren
  :commands paren-activate
  :config
  (paren-activate))

(use-package default-text-scale
  :bind
  (("C-M-=" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-c m b" . magit-blame-addition))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package git-gutter
  :disabled t
  :diminish git-gutter-mode
  :commands global-git-gutter-mode
  :bind
  (("C-c s n" . git-gutter:next-hunk)
   ("C-c s p" . git-gutter:previous-hunk))
  :config
  (global-git-gutter-mode +1))

(use-package pdf-tools
  :disabled t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (use-package pdf-annot :ensure nil)
  (use-package pdf-links :ensure nil)
  (use-package pdf-info :ensure nil)
  (use-package pdf-misc :ensure nil)
  (use-package pdf-sync :ensure nil)
  (use-package pdf-occur :ensure nil)
  (use-package pdf-outline :ensure nil)
  (use-package pdf-history :ensure nil)
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-misc-size-indication-minor-mode)
              (pdf-links-minor-mode)
              (pdf-isearch-minor-mode)
              (pdf-outline-minor-mode))))

(use-package eshell
  :ensure nil
  :bind
  ("C-x m" . eshell))

(use-package eshell-prompt-extras
  :after esh-opt
  :defines (eshell-highlight-prompt
            eshell-prompt-function)
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;; languages
;; Lisp

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(use-package redshank
  :disabled t
  :diminish redshank-mode
  :commands redshank-mode)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode)

(use-package lisp-mode
  :defer t
  :ensure nil
  :commands (lisp-indent-defform
             lisp-indent-specform)
  :defines calculate-lisp-indent-last-sexp
  :preface
  (defvar lisp-mode-hook-list
    '(lisp-mode-hook
      inferior-lisp-mode-hook
      lisp-interaction-mode-hook
      slime-repl-mode-hook))

  (defvar elisp-mode-hook-list
    '(emacs-lisp-mode-hook
      inferior-emacs-lisp-mode-hook
      ielm-mode-hook))

  (defun lisp-mode-setup-hook ()
    "Lisp mode."
    (auto-fill-mode +1)
    (eldoc-mode +1)
    (flycheck-mode +1)
    (paredit-mode +1)
    (paren-activate)
    (rainbow-delimiters-mode +1)
    ;;(redshank-mode +1)
    ;;(turn-on-eval-sexp-fu-flash-mode)
    (aggressive-indent-mode)
    (add-hook 'after-save-hook #'check-parens nil t))

  (defun elisp-mode-setup-hook ()
    "Elisp mode."
    (elisp-slime-nav-mode +1))

  (defun redefine-lisp-indent-function (indent-point state)
    "Redefine 'lisp-indent-function (INDENT-POINT STATE)."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  :init
  (dolist (hook lisp-mode-hook-list)
    (add-hook hook #'lisp-mode-setup-hook))

  (dolist (hook elisp-mode-hook-list)
    (add-hook hook #'lisp-mode-setup-hook)
    (add-hook hook #'elisp-mode-setup-hook)
    (add-hook hook (lambda ()
                     (setq-local lisp-indent-function
                                 #'redefine-lisp-indent-function)))))

;; c/c++

(use-package google-c-style
  :commands (google-set-c-style
             google-make-newline-indent)
  :functions c-langelem-pos
  :preface
  (defun c-mode-common-setup-hook ()
    (google-set-c-style)
    (google-make-newline-indent)
    (c-add-style "my-c-style"
                 '("Google"
                   (tab-width . 4)
                   (c-basic-offset . 4)
                   (indent-tabs-mode . nil)
                   (c-auto-newline . t)
                   (c-electric-flag . t)
                   (c-offsets-alist . ((access-label . -)
                                       (case-label . 0)
                                       (statement-case-intro . +)
                                       (statement-case-open . 0)
                                       ;;(arglist-cont-nonempty . +)
                                       ;;(arglist-cont . +)
                                       (member-init-intro . +)))))
    (c-set-style "my-c-style")
    (vr-c++-indentation-setup)
    (flycheck-mode +1)
    (modern-c++-font-lock-mode +1))

  (defun vr-c++-looking-at-lambda_as_param ()
    "Return t if text after point matches '[...](' or '[...]{'"
    (looking-at ".*[,(][ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

  (defun vr-c++-looking-at-lambda_in_uniform_init ()
    "Return t if text after point matches '{[...](' or '{[...]{'"
    (looking-at ".*{[ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

  (defun vr-c++-indentation-examine (langelem looking-at-p)
    (and (equal major-mode 'c++-mode)
         (ignore-errors
           (save-excursion
             (goto-char (c-langelem-pos langelem))
             (funcall looking-at-p)))))

  (defun vr-c++-indentation-setup ()
    (c-set-offset
     'block-close
     (lambda (langelem)
       (if (vr-c++-indentation-examine
            langelem
            #'vr-c++-looking-at-lambda_in_uniform_init)
           '-
         0)))

    (c-set-offset
     'statement-block-intro
     (lambda (langelem)
       (if (vr-c++-indentation-examine
            langelem
            #'vr-c++-looking-at-lambda_in_uniform_init)
           0
         '+)))

    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (vr-c++-indentation-examine
                 langelem
                 #'vr-c++-looking-at-lambda_as_param)
                0
              ad-do-it))))
  :init
  (add-hook 'c-mode-common-hook #'c-mode-common-setup-hook))

(use-package rtags
  :disabled t
  :bind
  (("M-." . rtags-find-symbol-at-point)
   ("M-," . rtags-location-stack-back))
  :init
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  :config
  (use-package helm-rtags
    :bind
    ("M--" . rtags-find-references-at-point)
    :init
    (setq rtags-display-result-backend 'helm)))

(use-package irony
  :disabled t
  :diminish irony-mode
  :commands irony-mode
  :preface
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package irony-eldoc
    :commands irony-mode
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc))
  (use-package company-irony
    :commands irony-mode)
  (use-package flycheck-irony
    :commands irony-mode
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :commands modern-c++-font-lock-mode
  :init
  (add-hook 'c++-mode-hook 'modern-c++-font-lock-mode))

(use-package cmake-mode
  :mode (("CMake[^/\\]*\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package cmake-project
  :commands (maybe-cmake-project-hook
             cmake-project-mode)
  :preface
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt")
        (cmake-project-mode)))
  :init
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook))

(use-package clang-format
  :commands clang-format-buffer)

(use-package gdb-mi
  :ensure nil
  :bind
  (("C-c g r" . gdb-restore-windows)
   ("C-c g m" . gdb-display-memory-buffer)
   ("C-c g a" . gdb-display-disassembly-buffer))
  :init
  ;; Force gdb-mi to not dedicate any windows
  (advice-add 'gdb-display-buffer
              :around (lambda (orig-fun &rest r)
                        (let ((window (apply orig-fun r)))
                          (set-window-dedicated-p window nil)
                          window)))
  (advice-add 'gdb-set-window-buffer
              :around (lambda (orig-fun name &optional ignore-dedicated window)
                        (funcall orig-fun name ignore-dedicated window)
                        (set-window-dedicated-p window nil)))
  :config
  (setq gdb-many-windows t
        gdb-non-stop-setting t
        gdb-show-changed-values t
        gdb-show-main t
        gdb-stack-buffer-addresses t
        gdb-stack-buffer-locations t
        gdb-switch-when-another-stopped t
        gdb-thread-buffer-addresses t
        gdb-thread-buffer-arguments t
        gdb-thread-buffer-locations t
        gdb-thread-buffer-verbose-names t
        gdb-use-colon-colon-notation t))

;; markdown

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

;; clojure

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
  :init
  (add-hook 'clojure-mode-hook #'lisp-mode-setup-hook)
  (add-hook 'clojure-mode-hook #'subword-mode)
  :config
  (use-package cljsbuild-mode)
  (use-package elein))

(use-package inf-clojure
  :commands inf-clojure-minor-mode
  :init
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))

(use-package cider
  :disabled t
  :commands cider-mode
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'cider-mode))
  :config
  (setq nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t)
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package ac-cider
  :disabled t
  :commands ac-cider-setup
  :init
  (with-eval-after-load 'cider-mode
    (add-hook 'cider-mode-hook #'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook #'ac-cider-setup))
  :config
  (with-eval-after-load 'auto-complete
    '(add-to-list 'ac-modes 'cider-mode)))

(use-package clj-refactor
  :commands (clj-refactor-mode
             cljr-add-keybindings-with-prefix)
  :preface
  (defun clj-refactor-setup-hook ()
    (clj-refactor-mode +1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'clj-refactor-setup-hook)))

(use-package flycheck-clojure
  :commands flycheck-clojure-setup
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'flycheck-clojure-setup)))

;; go

(use-package go-eldoc
  :commands go-eldoc-setup
  :preface
  (defun go-eldoc-setup-hook ()
    (go-eldoc-setup))
  :init
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode-hook #'go-eldoc-setup-hook)))

(use-package go-autocomplete
  :disabled t
  :defer t
  :init
  (with-eval-after-load 'auto-complete
    '(add-to-list 'ac-modes 'go-mode)))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :commands (gofmt-before-save
             godef-jump)
  :preface
  (defun go-mode-setup-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (setq compile-command "go build -v && go test -v && go vet")
    (add-hook 'before-save-hook #'gofmt-before-save))
  :init
  (add-hook 'go-mode-hook #'go-mode-setup-hook)
  :config
  (bind-keys :map go-mode-map
    ("M-." . godef-jump)
    ("C-c C-c" . compile)))

;; swift

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :init
  (add-hook 'swift-mode-hook 'flycheck-mode))

;; haskell

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

(use-package intero
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package dante
  :disabled t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; rust

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (add-hook 'rust-mode-hook #'flycheck-mode)
  :config
  (setq electric-indent-mode +1))

(use-package racer
  :commands (rust-mode
             racer-mode)
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :commands (rust-mode
             flycheck-rust-setup)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :commands (rust-mode
             cargo-minor-mode)
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

;; qml

(use-package qml-mode
  :mode ("\\.qml\\'" . qml-mode))

;;; org

(use-package org
  :ensure org-plus-contrib
  :commands org-babel-do-load-languages
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c b" . org-iswitchb)
   ("C-c k" . org-capture))
  :config
  (use-package org-capture
    :ensure nil
    :commands org-capture)
  (use-package ox-reveal
    :init
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
    (setq org-reveal-mathjax t))

  (use-package ob-plantuml
    :ensure nil
    :init
    (setq org-plantuml-jar-path (concat user-emacs-directory
                                        "lib/plantuml.jar")))

  ;; Add some new modules.
  (add-to-list 'org-modules 'org-habit)

  ;; Set up paths.
  (setq org-directory "~/org"
        ;; File for capturing new tasks.
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list (concat org-directory "/todo.org")
                               org-default-notes-file))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("NEXT" :foreground "#5E81AC" :weight bold)
          ("WAITING" :foreground "#D08770" :weight bold)
          ("HOLD" :foreground "#BF616A" :weight bold)
          ("CANCELLED" :foreground "#A3BE8C" :weight bold)))

  (setq org-capture-templates
        (let ((refile-file (concat org-directory "/notes.org")))
          `(("t" "todo" entry (file ,refile-file)
             "* TODO %?")
            ("n" "note" entry (file ,refile-file)
             "* %?"))))

  ;; Refile setup.
  (setq org-refile-targets '((org-agenda-files :level . 1))
        org-refile-use-outline-path 'file)

  ;; Do not split line when cursor in not at the end.
  (setq org-M-RET-may-split-line nil)

  ;; Highlight source code.
  (setq org-src-fontify-natively t)

  ;; Add a timestamp when a certain TODO item was finished.
  (setq org-log-done 'time)

  ;; Custom timestamp formats.
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats
        '("<%d-%m-%Y %a>" . "<%d-%m-%Y %a %H:%M>"))

  (setq org-ellipsis "...")

  ;; Align org tags before saving.
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'org-align-all-tags nil t)))

  ;; Org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (plantuml . t))))

;; Integration with beamer

(use-package ox-beamer
  :ensure nil
  :defer t
  :config
  ;; Don't ask me if this variable can be evaluated.
  (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv+block"
                 "O"
                 "\\begin{onlyenv}%a\\begin{block}{%h}"
                 "\\end{block}\\end{onlyenv}")))

(use-package org-journal
  :mode
  ("journal/[0-9]\\{8\\}$" . org-journal-mode)
  :config
  (setq org-journal-dir (concat org-directory "/journal/")))

(use-package org-bullets
  :commands org-bullets-mode
  :preface
  (defun org-bullets-mode-hook ()
    (org-bullets-mode +1))
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode-hook)
  :config
  (setq org-bullets-bullet-list '("•")))

;;; key bindings

(bind-key "C-c e" #'ediff-buffers)
(bind-key "C-c r" #'revert-buffer)
(bind-key "C-x k" #'kill-this-buffer)
(bind-key "C-x C-b" #'ibuffer)

;;; registers

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))

;;; Elapsed time

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)
;;; init.el ends here
