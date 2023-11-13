;;; setup-vertico.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :defines
  (vertico-map)
  :functions
  (vertico-mode)
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
   ("C-j" . vertico-insert)
   ("C-l" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'setup-vertico)
;;; setup-vertico.el ends here
