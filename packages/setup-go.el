;;; setup-go.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-eldoc
  :after go-mode
  :functions
  (go-eldoc-setup)
  :preface
  (defun go-eldoc-setup-hook ()
    (go-eldoc-setup))
  :hook
  (go-mode . go-eldoc-setup-hook))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :defines
  (gofmt-command
   go-mode-map)
  :functions
  (godef-jump
   gofmt-before-save)
  :preface
  (defun go-mode-setup-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (setq compile-command "go build -v && go test -v && go vet")
    (add-hook 'before-save-hook #'gofmt-before-save))
  :hook
  (go-mode . go-mode-setup-hook)
  :config
  (bind-keys :map go-mode-map
    ("M-." . godef-jump)
    ("C-c C-c" . compile)))

(provide 'setup-go)
;;; setup-go.el ends here
