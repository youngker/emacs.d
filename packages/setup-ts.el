;;; setup-ts.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :defines
  (treesit-auto-install)
  :functions
  (global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'setup-ts)
;;; setup-ts.el ends here
