;;; 10-appearance.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load-theme 'nord)

(set-face-attribute 'default nil :font "Monaco 16")

(blink-cursor-mode -1)
(global-font-lock-mode +1)
(global-hl-line-mode +1)
(tooltip-mode -1)

(setq inhibit-startup-message t)

(provide '10-appearance)
;;; 10-appearance.el ends here
