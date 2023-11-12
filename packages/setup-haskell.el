;;; setup-haskell.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :hook (haskell-mode . (lambda () (setq tab-width 4)))
  :mode ("\\.hs\\'" . haskell-mode))

(provide 'setup-haskell)
;;; setup-haskell.el ends here
