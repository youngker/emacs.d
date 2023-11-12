;;; setup-ts.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tree-sitter
  :diminish "ts"
  :init
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :demand t
  :after tree-sitter)

(provide 'setup-ts)
;;; setup-ts.el ends here
