;;; setup-rust.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cargo
  :diminish cargo-minor-mode
  :defines
  (cargo-minor-mode)
  :bind
  (:map cargo-minor-mode
   (("M-1" . cargo-process-build)
    ("M-2" . cargo-process-run)))
  :hook
  (rust-mode . cargo-minor-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq electric-indent-mode +1))

(provide 'setup-rust)
;;; setup-rust.el ends here
