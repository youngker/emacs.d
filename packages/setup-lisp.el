;;; setup-lisp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lisp-mode
  :defer t
  :ensure nil
  :commands (lisp-indent-defform
             lisp-indent-specform)
  :hook ((lisp-mode emacs-lisp-mode)
         . (lambda () (add-hook 'after-save-hook #'check-parens nil t)))
  :defines calculate-lisp-indent-last-sexp
  :preface
  (defvar elisp-mode-hook-list
    '(emacs-lisp-mode-hook
      ielm-mode-hook))

  (defun elisp-mode-setup-hook ()
    "Elisp mode."
    (elisp-slime-nav-mode +1))

  (defun redefine-lisp-indent-function (indent-point state)
    "Redefine 'lisp-indent-function (INDENT-POINT STATE)."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  :init
  (dolist (hook elisp-mode-hook-list)
    (add-hook hook #'elisp-mode-setup-hook)
    (add-hook hook (lambda ()
                     (setq-local lisp-indent-function
                                 #'redefine-lisp-indent-function)))))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :diminish
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . aggressive-indent-mode)
  :config
  (aggressive-indent-mode t))

(use-package elisp-slime-nav
  :diminish
  :commands elisp-slime-nav-mode)

(use-package rainbow-delimiters
  :hook
  ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . rainbow-delimiters-mode)
  :config
  (use-package color
    :ensure nil
    :demand t
    :config
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 20)))))

(use-package slime
  :commands slime-mode
  :config
  (setq inferior-lisp-program "sbcl" ;;"lx86cl64"
        slime-contribs '(slime-fancy)))

(use-package slime-company
  :after (slime company)
  :defines slime-company-competion
  :init
  (setq slime-company-competion 'fuzzy))

(use-package mic-paren
  :hook
  ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . paren-activate))

(provide 'setup-lisp)
;;; setup-lisp.el ends here
