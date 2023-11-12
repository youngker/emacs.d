;;; setup-shackle.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package shackle
  :init
  (shackle-mode)
  :config
  (setq shackle-rules
	'(("*Help*" :align t :select t)
	  ("*Google Translate*" :align t :select t)
	  ("*Embark Export*" :align t :select t)
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

(provide 'setup-shackle)
;;; setup-shackle.el ends here
