;;; setup-paredit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :diminish
  :defines
  (paredit-mode-map)
  :hook
  ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . paredit-mode)
  :config
  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))

(use-package paredit-everywhere
  :diminish
  :hook
  (prog-mode . paredit-everywhere-mode))

(provide 'setup-paredit)
;;; setup-paredit.el ends here
