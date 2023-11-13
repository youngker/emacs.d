;;; setup-eshell.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :ensure nil
  :defines
  (eshell-highlight-prompt
   eshell-prompt-function
   eshell-prompt-regexp)
  :functions
  (magit-get-current-branch
   eshell/pwd)
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
  :hook
  (eshell-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'my-eshell-prompt-function
        eshell-prompt-regexp "^$ "))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
