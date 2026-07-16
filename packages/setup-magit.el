;;; setup-magit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-c m b" . magit-blame-addition)
   ("C-c m l" . magit-log-buffer-file)))

(provide 'setup-magit)
;;; setup-magit.el ends here
