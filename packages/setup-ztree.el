;;; setup-ztree.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ztree
  :commands ztree-diff
  :defines
  (ztree-draw-unicode-lines
   ztreediff-mode-map)
  :config
  (set-face-attribute
   'ztreep-diff-model-add-face  nil :foreground "#87cefa")
  (setq ztree-draw-unicode-lines t)
  (bind-keys :map ztreediff-mode-map
    ("p" . previous-line)
    ("n" . next-line)))

(provide 'setup-ztree)
;;; setup-ztree.el ends here
