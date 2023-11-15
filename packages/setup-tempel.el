;;; setup-tempel.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tempel
  :defines
  (tempel-path)
  :bind
  (("C-<tab>" . tempel-expand))
  :config)

(use-package tempel-collection
  :after tempel)

(provide 'setup-tempel)
;;; setup-tempel.el ends here
