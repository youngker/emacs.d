;;; 99-packages.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; https://github.com/alphapapa/unpackaged.el#sort-sexps
(require 'cl-lib)
(defun sort-package (beg end)
  "Sort sexps in region (from BEG to END)."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
                     for end = (point-marker)
                     while sexp
                     ;; Collect the real string, then one used for sorting.
                     collect (cons (buffer-substring (marker-position start) (marker-position end))
                                   (save-excursion
                                     (goto-char (marker-position start))
                                     (skip-both)
                                     (buffer-substring (point) (marker-position end))))
                     into sexps
                     collect (cons start end)
                     into markers
                     finally return (list sexps markers))
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package auto-compile
  :functions
  (auto-compile-on-load-mode
   auto-compile-on-save-mode)
  :init
  (auto-compile-on-load-mode)
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-save-mode))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook
  (find-file . (lambda () (auto-revert-mode))))

(use-package avy
  :functions
  (avy-setup-default)
  :bind
  ("C-c ;" . avy-goto-char)
  :config
  (avy-setup-default))

(use-package company
  :diminish
  :defines
  (company-dabbrev-downcase
   company-echo-delay
   company-idle-delay
   company-minimum-prefix-length
   company-require-match
   company-selection-wrap-around
   company-tooltip-align-annotations
   company-tooltip-flip-when-above
   company-tooltip-limit
   company-transformers
   company-active-map)
  :functions
  (global-company-mode
   company-select-previous
   company-select-next
   company-complete-common-or-cycle)
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil
        company-echo-delay 0
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10
        company-transformers '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)
    ("TAB" . company-complete-common-or-cycle)))

(use-package compile
  :ensure nil
  :bind
  ("C-c c c" . show-compilation)
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook (compilation-filter . compilation-ansi-color-process-output))

(use-package default-text-scale
  :bind
  (("C-M-=" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package diff-hl
  :defines
  (diff-hl-side)
  :functions
  (global-diff-hl-mode
   diff-hl-margin-mode)
  :init
  (global-diff-hl-mode)
  :config
  (setq diff-hl-side 'left)
  (diff-hl-margin-mode))

(use-package diminish
  :commands diminish)

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-git-info
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))

(use-package eldoc
  :ensure nil
  :diminish
  :hook
  (prog-mode . eldoc-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  (paredit-mode . (lambda () (electric-pair-mode 0)))
  :custom
  (electric-pair-inhibit-predicate
   (lambda (c) (minibufferp))))

(use-package elogcat
  :commands elogcat)

(use-package eopengrok
  :bind
  (("C-c s i" . eopengrok-create-index)
   ("C-c s I" . eopengrok-create-index-with-enable-projects)
   ("C-c s d" . eopengrok-find-definition)
   ("C-c s f" . eopengrok-find-file)
   ("C-c s s" . eopengrok-find-reference)
   ("C-c s t" . eopengrok-find-text)
   ("C-c s h" . eopengrok-find-history)
   ("C-c s c" . eopengrok-find-custom)
   ("C-c s b" . eopengrok-resume)))

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :defines
  (exec-path-from-shell-check-startup-files)
  :functions
  (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-c =" . er/expand-region))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package google-translate
  :defines
  (google-translate-translation-directions-alist
   google-translate-backend-method)
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :preface
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  :init
  ;; (setq google-translate-output-destination 'popup)
  (setq google-translate-backend-method 'curl
        google-translate-translation-directions-alist
        '(("en" . "ko") ("ko" . "en"))))

(use-package helm-codesearch
  :disabled t
  :bind
  (("C-c h f" . helm-codesearch-find-file)
   ("C-c h t" . helm-codesearch-find-pattern)
   ("C-c h I" . helm-codesearch-create-csearchindex)))

(use-package ielm
  :ensure nil
  :commands ielm
  :hook
  (ielm-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setq ielm-header "")
  (setq ielm-prompt "=> "))

(use-package ksp-mode
  :mode ("\\.ksp\\'" . ksp-mode))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package page-break-lines
  :diminish
  :functions
  (global-page-break-lines-mode
   page-break-lines-mode)
  :init
  (global-page-break-lines-mode)
  :config
  (page-break-lines-mode))

(use-package plantuml-mode
  :commands plantuml-mode
  :defines
  (plantuml-executable-path
   plantuml-default-exec-mode)
  :config
  (setq plantuml-executable-path "plantuml"
        plantuml-default-exec-mode 'executable))

(use-package rainbow-mode
  :diminish
  :preface
  (defun enable-rainbow-mode ()
    (when (string-match "\\(color-theme-\\|-theme\\|init\\.el\\)" (buffer-name))
      (rainbow-mode)))
  :hook
  (emacs-lisp-mode . enable-rainbow-mode)
  ((css-mode html-mode sass-mode) . rainbow-mode))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :config
  (setq-default
   recentf-max-saved-items 100
   recentf-save-file (concat user-emacs-directory "recentf")))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode)
  :config
  (setq-default
   ;;save-place t
   save-place-file (concat user-emacs-directory "place")))

(use-package server
  :ensure nil
  :commands server-running-p
  :config
  (unless (server-running-p)
    (server-start)))

(use-package subword
  :ensure nil
  :diminish
  :hook
  (prog-mode . subword-mode))

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode))

(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

(use-package tuareg
  :mode ("\\.ml[ily]?$" . tuareg-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :defines
  (typescript-indent-level)
  :init
  (setq typescript-indent-level 2))

(use-package undo-tree
  :bind
  ("C-x u" . undo-tree-visualize)
  :diminish
  :commands global-undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package uniquify
  :ensure nil
  :defer t
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package visual-regexp
  :bind
  ("M-/" . vr/replace))

(use-package volatile-highlights
  :diminish
  :functions
  (volatile-highlights-mode)
  :init
  (volatile-highlights-mode))

(use-package which-key
  :diminish
  :commands which-key-mode
  :functions
  (which-key-setup-side-window-right)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package whitespace-cleanup-mode
  :functions
  (global-whitespace-cleanup-mode)
  :init
  (global-whitespace-cleanup-mode))

(use-package xclip
  :functions
  (xclip-mode)
  :if (and
       (not (display-graphic-p))
       (or
        (string-equal system-type "darwin")
        (and (string-equal system-type "gnu/linux")
             (executable-find "xclip"))))
  :config
  (xclip-mode +1))

(use-package xref
  :ensure nil
  :bind
  (:map xref--xref-buffer-mode-map
   ("C-j" . xref-show-location-at-point)))

(let ((dir (expand-file-name "packages" user-emacs-directory)))
  (dolist (file (directory-files dir t "\\.el$"))
    (load file)))

(provide '99-packages)
;;; 99-packages.el ends here
