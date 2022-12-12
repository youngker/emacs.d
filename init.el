;;; init.el --- youngker's configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.4.0
;; Keywords: convenience
;; Package-Requires: ((emacs "28.2"))

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
 gc-cons-threshold 100000000
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
 read-process-output-max (* 1024 1024)
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
                (font . "Monaco 16"))
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
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll))
(when (featurep 'ns)
  (global-set-key (kbd "<mouse-4>") (kbd "<wheel-up>"))
  (global-set-key (kbd "<mouse-5>") (kbd "<wheel-down>")))
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
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq package-enable-at-startup t)

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
   `(anzu-mode-line ((,class (:foreground, nord08))))
   `(anzu-mode-line-no-match ((,class (:foreground, nord11))))
   `(avy-lead-face ((,class (:background ,nord11 :foreground ,nord05))))
   `(avy-lead-face-0 ((,class (:background ,nord10 :foreground ,nord05))))
   `(avy-lead-face-1 ((,class (:background ,nord03 :foreground ,nord05))))
   `(avy-lead-face-2 ((,class (:background ,nord15 :foreground ,nord05))))
   `(aw-leading-char-face ((,class (:foreground ,nord11))))
   `(border ((,class (:foreground ,nord04))))
   `(buffer-menu-buffer ((,class (:foreground ,nord04 :weight normal))))
   `(button ((,class (:background ,nord00 :foreground ,nord08 :box (:line-width 2 :color ,nord04 :style sunken-button)))))
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
   `(completions-annotations ((,class (:foreground ,nord09))))
   `(completions-common-part ((,class (:foreground ,nord08 :weight normal))))
   `(completions-first-difference ((,class (:foreground ,nord11))))
   `(cursor ((,class (:background ,nord04))))
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
   `(diff-added ((,class (:foreground ,nord14))))
   `(diff-changed ((,class (:foreground ,nord13))))
   `(diff-context ((,class (:inherit default))))
   `(diff-file-header ((,class (:foreground ,nord08))))
   `(diff-function ((,class (:foreground ,nord07))))
   `(diff-header ((,class (:foreground ,nord09 :weight normal))))
   `(diff-hl-change ((,class (:background ,nord13))))
   `(diff-hl-delete ((,class (:background ,nord11))))
   `(diff-hl-insert ((,class (:background ,nord14))))
   `(diff-hunk-header ((,class (:foreground ,nord09 :background ,nord00))))
   `(diff-indicator-added ((,class (:foreground ,nord14))))
   `(diff-indicator-changed ((,class (:foreground ,nord13))))
   `(diff-indicator-removed ((,class (:foreground ,nord11))))
   `(diff-nonexistent ((,class (:foreground ,nord11))))
   `(diff-refine-added ((,class (:foreground ,nord14))))
   `(diff-refine-changed ((,class (:foreground ,nord13))))
   `(diff-refine-removed ((,class (:foreground ,nord11))))
   `(diff-removed ((,class (:foreground ,nord11))))
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
   `(elixir-atom-face ((,class (:foreground ,nord04 :weight normal))))
   `(elixir-attribute-face ((,class (:foreground ,nord12))))
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,nord14))))
   `(enh-ruby-op-face ((,class (:foreground ,nord09))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,nord13))))
   `(enh-ruby-regexp-face ((,class (:foreground ,nord13))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,nord14))))
   `(erm-syn-errline ((,class (:foreground ,nord11 :underline t))))
   `(erm-syn-warnline ((,class (:foreground ,nord13 :underline t))))
   `(eshell-ls-archive ((,class (:bold t :foreground ,nord12))))
   `(eshell-ls-backup ((,class (:foreground ,nord03))))
   `(eshell-ls-clutter ((,class (:foreground ,nord04))))
   `(eshell-ls-directory ((,class (:bold t :foreground ,nord11))))
   `(eshell-ls-executable ((,class (:foreground ,nord14))))
   `(eshell-ls-missing ((,class (:foreground ,nord07))))
   `(eshell-ls-product ((,class (:foreground ,nord13))))
   `(eshell-ls-readonly ((,class (:foreground ,nord03))))
   `(eshell-ls-special ((,class (:foreground ,nord13))))
   `(eshell-ls-symlink ((,class (:underline t :foreground ,nord08))))
   `(eshell-ls-unreadable ((,class (:foreground ,nord15))))
   `(evil-ex-info ((,class (:foreground ,nord08))))
   `(evil-ex-substitute-matches ((,class (:inherit isearch))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,nord09))))
   `(file-name-shadow ((,class (:inherit shadow))))
   `(font-latex-italic-face ((,class (:inherit italic))))
   `(font-latex-italic-face ((,class (:slant italic))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,nord09))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,nord04))))
   `(font-latex-math-face ((,class (:foreground ,nord08))))
   `(font-latex-normal-face ((,class (:inherit normal))))
   `(font-latex-normal-face ((,class (:inherit normal))))
   `(font-latex-script-char-face ((,class (:inherit font-lock-warning-face))))
   `(font-latex-sectioning-0-face ((,class (:foreground ,nord08 :weight normal))))
   `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-2-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-3-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-4-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-sectioning-5-face ((,class (:inherit font-latex-sectioning-0-face))))
   `(font-latex-string-face ((,class (:foreground ,nord14))))
   `(font-latex-string-face ((,class (:inherit font-lock-string-face))))
   `(font-latex-warning-face ((,class (:inherit font-lock-warning-face))))
   `(fringe ((,class (:foreground ,nord04 :background ,nord00))))
   `(git-gutter+-added ((,class (:foreground ,nord14))))
   `(git-gutter+-deleted ((,class (:foreground ,nord11))))
   `(git-gutter+-modified ((,class (:foreground ,nord13))))
   `(git-gutter:added ((,class (:foreground ,nord14))))
   `(git-gutter:deleted ((,class (:foreground ,nord11))))
   `(git-gutter:modified ((,class (:foreground ,nord13))))
   `(header-line ((,class (:foreground ,nord04 :background ,nord02))))
   `(help-argument-name ((,class (:foreground ,nord08))))
   `(highlight ((,class (:foreground ,nord08 :background ,nord02))))
   `(highlight-symbol-face ((,class (:foreground ,nord13 :background ,nord01))))
   `(hl-line ((,class (:background ,nord01))))
   `(info-menu-star ((,class (:foreground ,nord09))))
   `(isearch ((,class (:foreground ,nord00 :background ,nord08))))
   `(isearch-fail ((,class (:foreground ,nord11))))
   `(jdee-bug-breakpoint-cursor ((,class (:background ,nord02))))
   `(jdee-db-active-breakpoint-face ((,class (:background ,nord02 :weight normal))))
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
   `(js2-error ((,class (:foreground ,nord11))))
   `(js2-external-variable ((,class (:foreground ,nord04))))
   `(js2-function-call ((,class (:foreground ,nord08))))
   `(js2-function-param ((,class (:foreground ,nord04))))
   `(js2-instance-member ((,class (:foreground ,nord04))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,nord06))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,nord09))))
   `(js2-jsdoc-tag ((,class (:foreground ,nord07))))
   `(js2-jsdoc-type ((,class (:foreground ,nord07))))
   `(js2-jsdoc-value ((,class (:foreground ,nord03))))
   `(js2-object-property ((,class (:foreground ,nord04))))
   `(js2-private-function-call ((,class (:foreground ,nord08))))
   `(js2-private-member ((,class (:foreground ,nord04))))
   `(js2-warning ((,class (:foreground ,nord13))))
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
   `(link ((,class (:underline t))))
   `(link-visited ((,class (:underline t))))
   `(linum ((,class (:foreground ,nord03 :background ,nord00))))
   `(linum-relative-current-face ((,class (:foreground ,nord03 :background ,nord00))))
   `(magit-branch ((,class (:foreground ,nord07 :weight normal))))
   `(magit-diff-added ((,class (:foreground ,nord14))))
   `(magit-diff-added-highlight ((,class (:foreground ,nord14 :background ,nord02))))
   `(magit-diff-context-highlight ((,class (:background ,nord02))))
   `(magit-diff-file-header ((,class (:foreground ,nord08 :box (:color ,nord08)))))
   `(magit-diff-removed ((,class (:foreground ,nord11))))
   `(magit-diff-removed-highlight ((,class (:foreground ,nord11 :background ,nord02))))
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
   `(markdown-blockquote-face ((,class (:foreground ,nord03))))
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
   `(markdown-normal-face ((,class (:inherit normal))))
   `(markdown-reference-face ((,class (:inherit markdown-link-face))))
   `(markdown-url-face ((,class (:foreground ,nord04 :underline t))))
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
   `(mu4e-flagged-face ((,class (:foreground ,nord13))))
   `(mu4e-header-key-face ((,class (:foreground ,nord08))))
   `(mu4e-header-marks-face ((,class (:foreground ,nord09))))
   `(mu4e-highlight-face ((,class (:highlight))))
   `(mu4e-link-face ((,class (:underline t))))
   `(mu4e-title-face ((,class (:foreground ,nord08))))
   `(mu4e-unread-face ((,class (:foreground ,nord13 :weight normal))))
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
   `(next-error ((,class (:inherit error))))
   `(nobreak-space ((,class (:foreground ,nord03))))
   `(org-agenda-date ((,class (:foreground ,nord08 :underline nil))))
   `(org-agenda-date-today ((,class (:foreground ,nord08 :weight normal))))
   `(org-agenda-date-weekend ((,class (:foreground ,nord09))))
   `(org-agenda-dimmed-todo-face ((,class (:background ,nord13))))
   `(org-agenda-done ((,class (:foreground ,nord14))))
   `(org-agenda-done ((,class (:foreground ,nord14))))
   `(org-agenda-structure ((,class (:foreground ,nord09))))
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
   `(org-level-1 ((,class (:foreground ,nord08 :weight normal))))
   `(org-level-2 ((,class (:inherit org-level-1))))
   `(org-level-3 ((,class (:inherit org-level-1))))
   `(org-level-4 ((,class (:inherit org-level-1))))
   `(org-level-5 ((,class (:inherit org-level-1))))
   `(org-level-6 ((,class (:inherit org-level-1))))
   `(org-level-7 ((,class (:inherit org-level-1))))
   `(org-level-8 ((,class (:inherit org-level-1))))
   `(org-link ((,class (:underline t))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-scheduled ((,class (:foreground ,nord14))))
   `(org-scheduled-previously ((,class (:foreground ,nord13))))
   `(org-scheduled-today ((,class (:foreground ,nord08))))
   `(org-sexp-date ((,class (:foreground ,nord07))))
   `(org-special-keyword ((,class (:foreground ,nord09))))
   `(org-table ((,class (:foreground ,nord09))))
   `(org-todo ((,class (:foreground ,nord13 :weight normal))))
   `(org-upcoming-deadline ((,class (:foreground ,nord12))))
   `(org-verbatim ((,class (:foreground ,nord07))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:foreground ,nord13 :weight normal))))
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
   `(package-status-avail-obso ((,class (:foreground ,nord07 :slant italic))))
   `(package-status-available ((,class (:foreground ,nord07))))
   `(package-status-built-in ((,class (:foreground ,nord09))))
   `(package-status-dependency ((,class (:foreground ,nord08 :slant italic))))
   `(package-status-disabled ((,class (:foreground ,nord03))))
   `(package-status-external ((,class (:foreground ,nord12 :slant italic))))
   `(package-status-held ((,class (:foreground ,nord04 :weight normal))))
   `(package-status-incompat ((,class (:foreground ,nord11))))
   `(package-status-installed ((,class (:foreground ,nord07 :weight normal))))
   `(package-status-new ((,class (:foreground ,nord14))))
   `(package-status-unsigned ((,class (:underline ,nord13))))
   `(popup-tip-face ((,class (:background ,nord03 :foreground ,nord06))))
   `(powerline-active1 ((,class (:foreground ,nord04 :background ,nord01))))
   `(powerline-active2 ((,class (:foreground ,nord04 :background ,nord03))))
   `(powerline-evil-base-face ((,class (:foreground ,nord04))))
   `(powerline-evil-insert-face ((,class (:foreground ,nord00 :background ,nord04))))
   `(powerline-evil-normal-face ((,class (:background ,nord08))))
   `(powerline-evil-replace-face ((,class (:foreground ,nord00 :background ,nord09))))
   `(powerline-evil-visual-face ((,class (:foreground ,nord00 :background ,nord07))))
   `(powerline-inactive1 ((,class (:background ,nord02))))
   `(powerline-inactive2 ((,class (:background ,nord02))))
   `(query-replace ((,class (:foreground ,nord08 :background ,nord02))))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,nord07)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,nord08)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,nord09)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,nord10)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,nord12)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,nord13)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,nord14)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,nord15)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,nord11)))
   `(region ((,class (:foreground ,nord00 :background ,nord08))))
   `(scroll-bar ((,class (:background ,nord03))))
   `(secondary-selection ((,class (:background ,nord02))))
   `(show-paren-match-face ((,class (:foreground ,nord00 :background ,nord08))))
   `(show-paren-mismatch-face ((,class (:background ,nord11))))
   `(success ((,class (:foreground ,nord14))))
   `(term ((,class (:foreground ,nord04 :background ,nord00))))
   `(term-color-black ((,class (:foreground ,nord01 :background ,nord01))))
   `(term-color-blue ((,class (:foreground ,nord08 :background ,nord08))))
   `(term-color-cyan ((,class (:foreground ,nord07 :background ,nord07))))
   `(term-color-green ((,class (:foreground ,nord14 :background ,nord14))))
   `(term-color-magenta ((,class (:foreground ,nord15 :background ,nord15))))
   `(term-color-red ((,class (:foreground ,nord11 :background ,nord11))))
   `(term-color-white ((,class (:foreground ,nord05 :background ,nord05))))
   `(term-color-yellow ((,class (:foreground ,nord13 :background ,nord13))))
   `(tool-bar ((,class (:foreground ,nord04 :background ,nord03))))
   `(tooltip ((,class (:foreground ,nord00 :background ,nord04))))
   `(trailing-whitespace ((,class (:background ,nord11))))
   `(tty-menu-disabled-face ((,class (:foreground ,nord01))))
   `(tty-menu-enabled-face ((,class (:background ,nord02 foreground ,nord04))))
   `(tty-menu-selected-face ((,class (:foreground ,nord08 :underline t))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,nord08))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,nord04))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,nord09))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,nord04))))
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
   `(web-mode-html-attr-equal-face ((,class (:foreground ,nord04))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,nord07))))
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
   `(web-mode-variable-name-face ((,class (:foreground ,nord04))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
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
   `(xref-file-header ((,class (:foreground ,nord14))))
   `(xref-line-number ((,class (:foreground ,nord09))))
   `(xref-match ((,class (:foreground ,nord13))))))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq ns-function-modifier 'hyper))

;; https://github.com/alphapapa/unpackaged.el#sort-sexps
(defun my-sort-sexps (beg end)
  "Sort sexps in region (from BEG to END)."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
                     for end = (point-marker)
                     while sexp
                     ;; Collect the real string, then one used for sorting.
                     collect (cons (buffer-substring (marker-position start) (marker-position end))
                                   (save-excursion
                                     (goto-char (marker-position start))
                                     (skip-both)
                                     (buffer-substring (point) (marker-position end))))
                     into sexps
                     collect (cons start end)
                     into markers
                     finally return (list sexps markers))
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))

;;; use-package starts

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :diminish
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . aggressive-indent-mode)
  :config
  (aggressive-indent-mode t))

(use-package auto-compile
  :functions (auto-compile-on-load-mode auto-compile-on-save-mode)
  :init
  (auto-compile-on-load-mode)
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-save-mode))

(use-package autorevert
  :ensure nil
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook `(lambda () (auto-revert-mode +1))))

(use-package avy
  :bind
  ("C-c ;" . avy-goto-char)
  :config
  (avy-setup-default))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package cargo
  :diminish cargo-minor-mode
  :bind
  (:map cargo-minor-mode
   (("M-1" . cargo-process-build)
    ("M-2" . cargo-process-run)))
  :hook
  (rust-mode . cargo-minor-mode))

(use-package clang-format
  :commands clang-format-buffer
  :bind
  ("C-c c r" . clang-format-buffer))

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
  :config
  (use-package cljsbuild-mode)
  (use-package elein))

(use-package cmake-mode
  :mode (("CMake[^/\\]*\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package cmake-project
  :diminish
  :preface
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt")
        (cmake-project-mode)))
  :hook
  ((c-mode c++-mode) . maybe-cmake-project-hook))

(use-package company
  :diminish
  :defines company-dabbrev-downcase
  :functions global-company-mode
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil
        company-echo-delay 0
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10
        company-transformers '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)
    ("TAB" . company-complete-common-or-cycle)))

(use-package compile
  :ensure nil
  :bind
  ("C-c c c" . show-compilation)
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook (compilation-filter . compilation-ansi-color-process-output))

(use-package consult
  :after vertico
  :functions consult-line
  :preface
  (defun consult-occur ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  :bind
  (("C-x C-i" . consult-imenu)
   ("C-x f"   . consult-recent-file)
   ("C-x r l" . consult-bookmark)
   ("C-c h o" . consult-occur)
   ("C-c h m" . consult-multi-occur)
   ("C-c h r" . consult-ripgrep)
   ("C-c h b" . consult-buffer)
   ("C-c h l" . consult-flymake)
   ("C-c h e" . consult-compile-error)
   ("C-c h x" . consult-xref)
   ("M-g M-g" . consult-goto-line)
   :map minibuffer-local-map
   ("C-h" . consult-history))
  :custom
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref))

(use-package consult-codesearch
  :load-path "lisp"
  :bind
  ("C-c h f" . consult-codesearch-file)
  ("C-c h t" . consult-codesearch)
  ("C-c h I" . consult-codesearch-create-index))

(use-package default-text-scale
  :bind
  (("C-M-=" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :config
  (setq diff-hl-side 'left)
  (diff-hl-margin-mode))

(use-package diminish
  :commands diminish)

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-git-info
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))

(use-package eglot
  :hook
  ((c++-mode clojure-mode haskell-mode rust-mode) . eglot-ensure))

(use-package eldoc
  :ensure nil
  :diminish
  :hook
  (prog-mode . eldoc-mode))

(use-package elisp-slime-nav
  :diminish
  :commands elisp-slime-nav-mode)

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

(use-package eshell
  :ensure nil
  :defines (eshell-highlight-prompt eshell-prompt-function eshell-prompt-regexp)
  :functions (magit-get-current-branch eshell/pwd)
  :preface
  (defun my-eshell-prompt-function ()
    (require 'magit)
    (concat
     "\n"
     (propertize (user-login-name) 'face '(:foreground "#D08770")) " at "
     (propertize (system-name) 'face '(:foreground "#EBCB8B")) " in "
     (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "#A3BE8C"))
     (and (magit-get-current-branch)
          (concat " on " (propertize (magit-get-current-branch) 'face '(:foreground "#B48EAD")))) "\n$ "))
  :bind
  ("C-x m" . eshell)
  :init
  (add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'my-eshell-prompt-function
        eshell-prompt-regexp "^$ "))

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :defines exec-path-from-shell-check-startup-files
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-c =" . er/expand-region))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package gdb-mi
  :ensure nil
  :bind
  (("C-c c d" . gdb)
   ("C-c g r" . gdb-restore-windows)
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

(use-package go-eldoc
  :commands go-eldoc-setup
  :preface
  (defun go-eldoc-setup-hook ()
    (go-eldoc-setup))
  :init
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode-hook #'go-eldoc-setup-hook)))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun go-mode-setup-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (setq compile-command "go build -v && go test -v && go vet")
    (add-hook 'before-save-hook #'gofmt-before-save))
  :hook
  (go-mode . go-mode-setup-hook)
  :config
  (bind-keys :map go-mode-map
    ("M-." . godef-jump)
    ("C-c C-c" . compile)))

(use-package google-c-style
  :preface
  (defun c-mode-common-setup-hook ()
    (google-set-c-style)
    (google-make-newline-indent)
    (c-add-style "my-c-style"
                 '("Google"
                   (tab-width . 4)
                   (c-basic-offset . 4)
                   (indent-tabs-mode . nil)
                   (c-auto-newline . nil)
                   (c-electric-flag . t)
                   (c-offsets-alist . ((access-label . -)
                                       (case-label . 0)
                                       (inlambda . 0)
                                       (statement-case-intro . +)
                                       (statement-case-open . 0)
                                       (member-init-intro . +)))))
    (c-set-style "my-c-style")
    (local-unset-key (kbd "C-c ."))
    (flymake-mode +1)
    (modern-c++-font-lock-mode +1))
  :hook
  (c-mode-common . c-mode-common-setup-hook))

(use-package google-translate
  :defines google-translate-translation-directions-alist
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :preface
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  :init
  ;; (setq google-translate-output-destination 'popup)
  (setq google-translate-backend-method 'curl
        google-translate-translation-directions-alist
        '(("en" . "ko") ("ko" . "en"))))

(use-package haskell-mode
  :hook (haskell-mode . (lambda () (setq tab-width 4)))
  :mode ("\\.hs\\'" . haskell-mode))

(use-package highlight-symbol
  :diminish
  :bind
  ("C-c ." . highlight-symbol)
  :hook
  ((prog-mode html-mode css-mode) . highlight-symbol-mode)
  ((prog-mode html-mode css-mode) . highlight-symbol-nav-mode)
  :config
  (setq highlight-symbol-colors
        '("#BF616A" "#D08770" "#EBCB8B" "#A3BE8C" "#B48EAD"
          "#5E81AC" "#81A1C1" "#88C0D0" "#8FBCBB"))
  (setq highlight-symbol-foreground-color "#D8DEE9"))

(use-package helm-codesearch
  :disabled t
  :bind
  (("C-c h f" . helm-codesearch-find-file)
   ("C-c h t" . helm-codesearch-find-pattern)
   ("C-c h I" . helm-codesearch-create-csearchindex)))

(use-package ielm
  :ensure nil
  :commands ielm
  :hook
  (ielm-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setq ielm-header "")
  (setq ielm-prompt "=> "))

(use-package inf-clojure
  :disabled t
  :hook
  (clojure-mode . inf-clojure-minor-mode))

(use-package lisp-mode
  :defer t
  :ensure nil
  :commands (lisp-indent-defform
             lisp-indent-specform)
  :hook ((lisp-mode emacs-lisp-mode)
         . (lambda () (add-hook 'after-save-hook #'check-parens nil t)))
  :defines calculate-lisp-indent-last-sexp
  :preface
  (defvar elisp-mode-hook-list
    '(emacs-lisp-mode-hook
      ielm-mode-hook))

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
  (dolist (hook elisp-mode-hook-list)
    (add-hook hook #'elisp-mode-setup-hook)
    (add-hook hook (lambda ()
                     (setq-local lisp-indent-function
                                 #'redefine-lisp-indent-function)))))

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-c m b" . magit-blame-addition))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package mic-paren
  :commands paren-activate
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . paren-activate)
  :config
  (paren-activate))

(use-package modern-cpp-font-lock
  :diminish
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package org
  :ensure nil
  :commands org-babel-do-load-languages
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o l" . org-store-link)
   ("C-c o b" . org-iswitchb)
   ("C-c o k" . org-capture))
  :config
  (use-package org-capture
    :ensure nil
    :commands org-capture)
  (use-package ox-reveal
    :defines org-reveal-mathjax
    :init
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
    (setq org-reveal-mathjax t))

  (use-package ob-plantuml
    :ensure nil
    :init
    (setq org-plantuml-jar-path (concat user-emacs-directory
                                        "lib/plantuml.jar")))

  (use-package org-tempo
    :ensure nil
    :init
    (setq org-structure-template-alist
          (eval (car (get 'org-structure-template-alist 'standard-value)))))

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
     (dot . t)
     (plantuml . t))))

(use-package org-bullets
  :commands org-bullets-mode
  :preface
  (defun org-bullets-mode-hook ()
    (org-bullets-mode +1))
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode-hook)
  :config
  (setq org-bullets-bullet-list '("•")))

(use-package org-journal
  :mode
  ("journal/[0-9]\\{8\\}$" . org-journal-mode)
  :config
  (setq org-journal-dir (concat org-directory "/journal/")))

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

(use-package page-break-lines
  :diminish
  :init
  (global-page-break-lines-mode)
  :config
  (page-break-lines-mode))

(use-package paredit
  :diminish
  :commands paredit-mode
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . paredit-mode)
  :config
  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))

(use-package paredit-everywhere
  :diminish
  :hook
  ((prog-mode css-mode) . paredit-everywhere-mode))

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

(use-package plantuml-mode
  :commands plantuml-mode
  :config
  (setq plantuml-jar-path (concat user-emacs-directory
                                  "lib/plantuml.jar"))
  (setq plantuml-default-exec-mode 'jar))

(use-package qml-mode
  :mode ("\\.qml\\'" . qml-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . rainbow-delimiters-mode)
  :config
  (use-package color
    :ensure nil
    :commands color-saturate-name
    :demand t
    :config
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 20)))))

(use-package rainbow-mode
  :diminish
  :preface
  (defun enable-rainbow-mode ()
    (when (string-match "\\(color-theme-\\|-theme\\|init\\.el\\)" (buffer-name))
      (rainbow-mode)))
  :hook
  (emacs-lisp-mode . enable-rainbow-mode)
  ((css-mode html-mode sass-mode) . rainbow-mode))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :config
  (setq-default
   recentf-max-saved-items 100
   recentf-save-file (concat user-emacs-directory "recentf")))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq electric-indent-mode +1))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode)
  :config
  (setq-default
   ;;save-place t
   save-place-file (concat user-emacs-directory "place")))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package shackle
  :init
  (shackle-mode)
  :config
  (setq shackle-rules
        '(("*Help*" :align t :select t)
          ("*Google Translate*" :align t :select t)
          (" *undo-tree*" :align right :size 0.1)
          ((grep-mode compilation-mode) :align t :select t)
          ("*xref*" :popup t :regexp t :align t :select t)
          ("*Cargo .*" :popup t :regexp t :align t :select t)
          ("*Flymake diagnostics .*" :popup t :regexp t :align t :select t)
          ("\\`\\*cider-repl .*" :regexp t :align t :size 0.2)
          ((inferior-scheme-mode "*shell*" "*eshell*") :popup t :align t))
        shackle-default-rule '(:select t)
        shackle-default-size 0.4
        shackle-inhibit-window-quit-on-same-windows t))

(use-package slime
  :commands slime-mode
  :config
  (setq inferior-lisp-program "sbcl" ;;"lx86cl64"
        slime-contribs '(slime-fancy)))

(use-package slime-company
  :after (slime company)
  :defines slime-company-competion
  :init
  (setq slime-company-competion 'fuzzy))

(use-package subword
  :ensure nil
  :diminish
  :hook
  (prog-mode . subword-mode))

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode))

(use-package term
  :ensure nil
  :preface
  ;; from xterm-frobs.el
  (defvar xterm-screen-dcs-encapsulation
    (not (null (or (getenv "STY")
                   (save-match-data
                     (string-match "^screen\\(\\|-.*\\)$"
                                   (or (getenv "TERM") "dumb")))))))

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

(use-package tree-sitter
  :diminish "ts"
  :init
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :demand t
  :after tree-sitter)

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :init
  (setq typescript-indent-level 2))

(use-package undo-tree
  :bind
  ("C-x u" . undo-tree-visualize)
  :diminish
  :commands global-undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package uniquify
  :ensure nil
  :defer t
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package vertico
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
   ("C-j" . vertico-insert)
   ("C-l" . vertico-directory-delete-word)))

(use-package visual-regexp
  :bind
  ("M-/" . vr/replace))

(use-package volatile-highlights
  :diminish
  :functions volatile-highlights-mode
  :init (volatile-highlights-mode))

(use-package which-key
  :diminish
  :functions which-key-setup-side-window-right
  :commands which-key-mode
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package whitespace-cleanup-mode
  :functions global-whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode))

(use-package xref
  :ensure nil
  :bind
  (:map xref--xref-buffer-mode-map
   ("C-j" . xref-show-location-at-point)))

(use-package yasnippet
  :diminish yas-minor-mode
  :functions yas-global-mode
  :init
  (yas-global-mode)
  :bind
  ("C-M-y" . company-yasnippet)
  :config
  (use-package yasnippet-snippets)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(use-package ztree
  :commands ztree-diff
  :config
  (set-face-attribute
   'ztreep-diff-model-add-face  nil :foreground "#87cefa")
  (setq ztree-draw-unicode-lines t)
  (bind-keys :map ztreediff-mode-map
    ("p" . previous-line)
    ("n" . next-line)))

;;; use-package ends

;;; key bindings

(bind-key "C-c e" #'ediff-buffers)
(bind-key "C-c r" #'revert-buffer)
(bind-key "C-x k" #'kill-this-buffer)
(bind-key "C-c n" #'display-line-numbers-mode)
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
