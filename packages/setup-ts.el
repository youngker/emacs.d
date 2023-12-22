;;; setup-ts.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :defines
  (treesit-auto-install
   c-ts-mode-map)
  :functions
  (global-treesit-auto-mode)
  :bind (:map c-ts-mode-map
         ("C-c ." . nil))
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'setup-ts)
;;; setup-ts.el ends here
