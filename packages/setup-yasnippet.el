;;; setup-yasnippet.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :disabled t
  :diminish yas-minor-mode
  :defines
  (yas-verbosity
   yas-wrap-around-region)
  :functions
  (yas-global-mode)
  :init
  (yas-global-mode)
  :bind
  ("C-M-y" . company-yasnippet)
  :config
  (use-package yasnippet-snippets)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here
