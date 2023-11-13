;;; setup-magit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defines
  (magit-completing-read-function)
  :bind
  (("C-x g"   . magit-status)
   ("C-c m b" . magit-blame-addition))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(provide 'setup-magit)
;;; setup-magit.el ends here
