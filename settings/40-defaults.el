;;; 40-defaults.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
 global-auto-revert-non-file-buffers t
 history-length 1000
 indent-tabs-mode nil
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

(auto-compression-mode +1)
(global-auto-revert-mode +1)
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

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq ns-function-modifier 'hyper))

(provide '40-defaults)
;;; 40-defaults.el ends here
