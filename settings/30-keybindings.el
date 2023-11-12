;;; 30-keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(bind-key "C-c e" #'ediff-buffers)
(bind-key "C-c r" #'revert-buffer)
(bind-key "C-x k" #'kill-this-buffer)
(bind-key "C-c n" #'display-line-numbers-mode)
(bind-key "C-x C-b" #'ibuffer)

(provide '30-keybindings)
;;; 30-keybindings.el ends here
