;;; setup-consult.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :after vertico
  :functions consult-line
  :preface
  (defun consult-occur ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  :bind
  (("C-x C-i" . consult-imenu)
   ("C-x b"   . consult-buffer)
   ("C-x f"   . consult-recent-file)
   ("C-x r l" . consult-bookmark)
   ("C-c h o" . consult-occur)
   ("C-c h m" . consult-multi-occur)
   ("C-c h r" . consult-ripgrep)
   ("C-c h l" . consult-flymake)
   ("C-c h e" . consult-compile-error)
   ("C-c h x" . consult-xref)
   ("M-g M-g" . consult-goto-line))
  :custom
  (consult-async-refresh-delay 0.1)
  (consult-async-input-throttle 0)
  (consult-async-input-debounce 0)
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref))

(use-package consult-codesearch
  :bind
  (("C-c h f" . consult-codesearch-find-file)
   ("C-c h t" . consult-codesearch)
   ("C-c h I" . consult-codesearch-build-index)
   :map minibuffer-local-map
   ("C-c h" . consult-history)
   ("C-c s" . embark-export)))

(use-package embark-consult
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'setup-consult)
;;; setup-consult.el ends here
