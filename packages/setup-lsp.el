;;; setup-lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure nil
  :hook
  ((c-mode
    c-ts-mode
    c++-mode
    c++-ts-mode
    clojure-mode
    cmake-mode
    go-mode
    haskell-mode
    java-mode
    nix-mode
    rust-mode
    tuareg-mode) . eglot-ensure))

(provide 'setup-lsp)
;;; setup-lsp.el ends here
