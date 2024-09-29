;;; setup-terminal.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package term
  :ensure nil
  :preface
  ;; from xterm-frobs.el
  (defvar xterm-screen-dcs-encapsulation
    (not (null (or (getenv "STY")
                   (save-match-data
                     (string-match "^screen\\(\\|-.*\\)$"
                                   (or (getenv "TERM") "dumb")))))))

  (defun xterm-send-escape-sequence (string)
    (cond ((and xterm-screen-dcs-encapsulation
                (save-match-data (string-match "\e[P\\\\]" string)))
           (save-match-data
             (let ((pos 0)
                   (substrings nil))
               (while (string-match "\e\\(P\\|\\\\\\)" string pos)
                 (setq substrings
                       (cons "\e\\"
                             (cons (substring string pos (match-beginning 1))
                                   (cons "\eP" substrings))))
                 (setq pos (match-beginning 1)))
               (setq substrings (cons (substring string pos) substrings))
               (setq string (mapconcat 'identity (nreverse substrings) "")))))
          (xterm-screen-dcs-encapsulation
           (setq string (format "\eP%s\e\\" string))))
    (send-string-to-terminal string))

  (defun terminal-title-hook ()
    (if (not window-system)
        (xterm-send-escape-sequence
         (format "\e]2;%s\a"
                 (concat (system-name) ": "
                         (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name))
                           (buffer-name)))))))
  :init
  (setq frame-title-format
        '((:eval (system-name)) ": "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))
  :hook
  (post-command . terminal-title-hook))

(use-package vterm
  :ensure nil
  :hook
  (vterm-mode . (lambda ()
                  (setq-local global-hl-line-mode nil)
                  (setq show-trailing-whitespace nil))))

(provide 'setup-terminal)
;;; setup-terminal.el ends here
