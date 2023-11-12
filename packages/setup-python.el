;;; setup-python.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-black
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(provide 'setup-python)
;;; setup-python.el ends here
