;;; init.el --- youngker's configuration

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.2.0
;; Keywords: convenience
;; Package-Requires: ((emacs "24.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; youngker's Emacs configuration
;;
;; See documentation on https://github.com/youngker/.emacs.d

;;; Code:

(defconst start-time (current-time))


;;; Basic preferences

;; theme
(defvar my-background-color "#000000")
(defvar my-foreground-color "#ffffff")

(defun start-my-theme ()
  "Start my theme."
  (deftheme my-theme "youngker's theme")
  (custom-theme-set-faces
   'my-theme
   `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
                  :background ,my-background-color
                  :foreground ,my-foreground-color :strike-through nil
                  :overline nil :underline nil :slant normal :weight normal
                  :width normal :family "Monaco"))))
   '(region ((nil (:background "#8b1c3b" :foreground "#e79ea1"))))
   '(hl-line ((((class color) (background dark)) (:background "#222222"))
              (((class color) (background light)) (:background "#f4f4f4"))))
   '(yas-field-highlight-face ((nil (:background "#333399"))))
   '(js2-function-param-face ((t (:foreground "#eedd82"))))
   '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
   '(show-paren-match ((nil (:background "#333399"))))
   '(show-paren-mismatch ((((class color)) (:background "#cc1b00"))))
   '(eval-sexp-fu-flash ((t (:background "#8b1c3b" :foreground "#e79ea1"))))
   '(helm-source-header ((((class color) (background dark))
                          (:background "#000000" :foreground "#ff7f24"))
                         (((class color) (background light))
                          (:background "#ffffff" :foreground "#ff7f24"))))
   '(helm-candidate-number ((t (:background nil :foreground "#8b1c3b" :bold t))))
   '(helm-selection ((((class color) (background dark)) (:background "#373a40"))
                     (((class color) (background light)) (:background "#f4f4f4"))))
   '(helm-visible-mark ((t (:background "#444444" :forground "#ffffff"))))
   '(helm-match ((((class color) (background dark))
                  (:background "#000000" :foreground "#333399"))
                 (((class color) (background light))
                  (:background "#ffffff" :foreground "#ff6666"))))
   '(helm-match-item ((t (:background "#333399" :foreground "#ffffff"))))
   '(helm-grep-file ((t (:foreground "#87cefa" :background "#000000"))))
   '(helm-grep-lineno ((t (:foreground "#7ffcd4" :background "#000000"))))
   '(helm-swoop-target-line-face ((t (:background "#222222"))))
   '(helm-swoop-target-word-face ((t (:background "#333399" :foreground "#ffffff"))))
   '(ac-candidate-face ((t (:background "#1a1a1a" :foreground "#a6a6a6"))))
   '(ac-selection-face ((t (:background "#2e2e2e" :foreground "#a6a6a6"))))
   '(popup-isearch-match ((t (:background "#000000" :foreground "#fe2f92"))))
   '(popup-tip-face ((t (:background "#1a1a1a" :foreground "#a6a6a6"))))
   '(popup-scroll-bar-foreground-face ((t (:background "#6e6e6e"))))
   '(popup-scroll-bar-background-face ((t (:background "#383838"))))
   `(ivy-current-match ((t (:foreground "#ff7f24"
                            :background ,my-background-color :weight bold))))))


(defun toggle-dark/light-theme ()
  "Toggle dark/light theme."
  (interactive)
  (if (string= (face-attribute 'default :background) "#ffffff")
      (setq my-background-color "#000000"
            my-foreground-color "#ffffff")
    (setq my-background-color "#ffffff"
          my-foreground-color "#000000"))
  (start-my-theme))

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (set-face-attribute 'default nil :height 180)
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; sane defaults
(setq-default
 auto-revert-verbose nil
 blink-matching-paren nil
 column-number-mode t
 confirm-nonexistent-file-or-buffer nil
 delete-by-moving-to-trash nil
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 electric-indent-mode nil
 fill-column 80
 gc-cons-threshold 50000000
 global-auto-revert-non-file-buffers t
 history-length 1000
 indent-tabs-mode nil
 inhibit-startup-message t
 initial-major-mode 'text-mode
 large-file-warning-threshold 100000000
 make-backup-files nil
 next-error-highlight t
 next-error-highlight-no-select t
 query-replace-highlight t
 require-final-newline t
 ring-bell-function #'ignore
 scroll-conservatively 100000
 scroll-margin 0
 scroll-preserve-screen-position 1
 sentence-end-double-space nil
 shift-select-mode nil
 tab-always-indent 'complete
 transient-mark-mode t
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell nil
 x-select-enable-clipboard t)

(start-my-theme)
(auto-compression-mode +1)
(blink-cursor-mode -1)
(global-auto-revert-mode +1)
(global-font-lock-mode +1)
(global-hl-line-mode +1)
(savehist-mode +1)
(show-paren-mode +1)
(winner-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; locale
(set-language-environment "Korean")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;; Bootstrap `use-package'

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize 'noactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (package-initialize)
(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish nil t)
(setq use-package-verbose t)
(setq use-package-always-ensure t)


;;;

(use-package server
  :ensure nil
  :commands server-running-p
  :config
  (unless (server-running-p)
    (server-start)))

(use-package auto-compile
  :commands (auto-compile-on-load-mode
             auto-compile-on-save-mode)
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands global-page-break-lines-mode
  :config
  (global-page-break-lines-mode)
  (turn-on-page-break-lines-mode))

(use-package whitespace-cleanup-mode
  :commands global-whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(use-package smooth-scrolling
  :disabled t)

(use-package subword
  :diminish subword-mode
  :commands subword-mode
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :config
  (global-subword-mode +1))

(use-package recentf
  :commands recentf-mode
  :config
  (recentf-mode +1)
  (setq-default
   recentf-max-saved-items 100
   recentf-save-file (concat user-emacs-directory "recentf")))

(use-package saveplace
  :after ivy
  :config
  (setq-default
   save-place t
   save-place-file (concat user-emacs-directory "place")))

(use-package uniquify
  :ensure nil
  :defer t
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


;;; Mac

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :defines (exec-path-from-shell-check-startup-files
            mac-option-modifier
            mac-command-modifier
            ns-function-modifier)
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(when (equal system-type 'darwin)
  (menu-bar-mode +1)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))


;;; Ido

(use-package ido
  :disabled t
  :bind
  (("C-x C-f" . ido-find-file)
   ("C-x f"   . ido-recent-file)
   ("C-x b"   . ido-switch-buffer)
   ("C-x B"   . ido-switch-buffer-other-window))
  :commands (ido-fallback-command
             ido-complete
             ido-everywhere
             ido-select-text
             ido-exit-minibuffer)
  :preface
  (defun ido-recent-file ()
    "Find a recent file."
    (interactive)
    (recentf-mode +1)
    (let ((file (ido-completing-read
                 "Choose recent file: "
                 (mapcar 'abbreviate-file-name recentf-list) nil t)))
      (when file
        (find-file file))))
  :config
  (ido-mode +1)
  (ido-everywhere +1)
  (icomplete-mode +1)
  (setq ido-auto-merge-work-directories-length -1
        ido-case-fold nil
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-use-faces nil
        ido-use-filename-at-point nil)
  (define-key ido-file-completion-map (kbd "C-\\") 'backward-kill-word)

  (use-package flx-ido
    :demand t
    :commands (flx-ido-mode flx-ido-reset flx-ido-debug flx-ido-match)
    :config
    (flx-ido-mode +1))

  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

  (use-package ido-ubiquitous
    :config
    (ido-ubiquitous-mode +1)))


;;; ivy

(use-package ivy
  :diminish ivy-mode
  :preface
  (defun ivy-setup-hook ()
    (auto-compile-on-load-mode)
    (dired-details-install)
    (global-auto-complete-mode)
    (global-diff-hl-mode)
    (global-page-break-lines-mode)
    (global-whitespace-cleanup-mode)
    (popwin-mode)
    (recentf-mode)
    (server-running-p)
    (volatile-highlights-mode)
    (which-key-mode)
    (yas-global-mode))
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-c C-r" . ivy-resume))
  :init
  (add-hook 'ivy-mode-hook #'ivy-setup-hook)
  :config
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-arrow))

(use-package counsel
  :bind
  (("C-x C-f" . counsel-find-file)
   ("C-x C-i" . counsel-imenu)
   ("C-x f" . counsel-recentf)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)))

(use-package swiper
  :bind
  ("M-i" . swiper))


;;; helm

(use-package helm-codesearch
  :bind
  (("C-c h f" . helm-codesearch-find-file)
   ("C-c h t" . helm-codesearch-find-pattern)
   ("C-c h I" . helm-codesearch-create-csearchindex)
   ("C-c h b" . helm-resume)))

(use-package helm
  :disabled t
  :bind
  (("C-x C-i" . helm-imenu)
   ("C-c g"   . helm-find-files)
   ("M-i"     . helm-swoop)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   ("C-c h w" . helm-descbinds)
   ("C-c h f" . helm-codesearch-find-file)
   ("C-c h t" . helm-codesearch-find-pattern)
   ("C-c h I" . helm-codesearch-create-csearchindex)
   ("C-c h b" . helm-resume)
   ("M-y"     . helm-show-kill-ring))
  :config
  (use-package helm-command :ensure nil)
  (use-package helm-semantic :ensure nil)
  (use-package helm-ring :ensure nil)
  (use-package helm-config
    :ensure nil
    :demand t
    :commands (async-bytecomp-get-allowed-pkgs
               async-byte-recompile-directory))
  (use-package helm-descbinds)
  (use-package helm-swoop)
  (use-package helm-codesearch)

  (bind-keys :map helm-map
    ("<tab>" . helm-execute-persistent-action)
    ("C-i"   . helm-execute-persistent-action)
    ("C-z"   . helm-select-action))

  ;; (helm-autoresize-mode +1)

  (setq helm-M-x-fuzzy-match        t
        helm-M-x-requires-pattern   nil
        ;; helm-autoresize-max-height  30
        ;; helm-autoresize-min-height 30
        helm-buffers-fuzzy-matching t
        helm-display-header-line    nil
        helm-ff-skip-boring-files   t
        helm-idle-delay             0.0
        helm-imenu-fuzzy-match      t
        helm-input-idle-delay       0.01
        helm-quick-update           t
        helm-recentf-fuzzy-match    t
        helm-semantic-fuzzy-match   t)

  (defvar helm-source-header-default-background
    (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground
    (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box
    (face-attribute 'helm-source-header :box))

  (defun helm-toggle-header-line ()
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height 1.0)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))

  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line))


;;; Tool

(use-package smex
  :disabled t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook `(lambda () (auto-revert-mode +1))))

(use-package avy
  :bind
  ("C-c ;" . avy-goto-char)
  :commands avy-setup-default
  :config
  (avy-setup-default))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package highlight-tail
  :disabled t
  :diminish highlight-tail-mode
  :config
  (setq highlight-tail-steps 16)
  (highlight-tail-mode))

(use-package win-switch
  :disabled t
  :bind
  ("C-x o" . win-switch-dispatch)
  :commands win-switch-set-keys
  :config
  (setq win-switch-feedback-background-color "#6c96af")
  (setq win-switch-feedback-foreground-color "#000000")
  (setq win-switch-window-threshold 1)
  (setq win-switch-idle-time 0.7)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window))

(use-package popwin
  :commands popwin-mode
  :config
  (popwin-mode +1)
  (setq popwin:special-display-config
        '(("*Help*"
           :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*compilation*"
           :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Shell Command Output*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*Async Shell Command*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*undo-tree*"
           :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*ert*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*grep*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*"
           :dedicated t :position bottom :stick t :noselect nil)
          ("^\*WoMan.+\*$"
           :regexp t :position bottom))))

(use-package undo-tree
  :bind
  ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package ztree
  :commands ztree-diff
  :config
  (set-face-attribute
   'ztreep-diff-model-add-face  nil :foreground "#87cefa")
  (setq ztree-draw-unicode-lines t)
  (bind-keys :map ztreediff-mode-map
    ("p" . previous-line)
    ("n" . next-line)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-global-mode
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (yas-global-mode +1))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :diminish aggressive-indent-mode
  :config
  (aggressive-indent-mode t))

(use-package expand-region
  :bind
  ("C-c =" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :config
  (which-key-mode +1))

(use-package visual-regexp
  :bind
  ("M-/" . vr/replace))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands
  (highlight-symbol-mode highlight-symbol-nav-mode)
  :bind
  ("C-c c" . highlight-symbol)
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook #'highlight-symbol-mode)
    (add-hook hook #'highlight-symbol-nav-mode)))

(use-package diff-hl
  :commands global-diff-hl-mode
  :config
  (global-diff-hl-mode +1)
  (setq diff-hl-draw-borders nil))

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

(use-package google-translate
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :config
  (use-package google-translate-smooth-ui :ensure nil)
  ;; (setq google-translate-output-destination 'popup)
  (setq google-translate-translation-directions-alist
        '(("en" . "ko") ("ko" . "en"))))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :preface
  (defun enable-rainbow-mode ()
    (when (string-match "\\(color-theme-\\|-theme\\|init\\.el\\)" (buffer-name))
      (rainbow-mode +1)))
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook #'rainbow-mode)))


;;; Syntax check

(use-package flycheck
  :commands flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon
    'flycheck-show-error-at-point))


;;; Lisp tools

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config
  (use-package color :ensure nil
    :commands color-saturate-name
    :demand t
    :config
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 20)))))

(use-package paredit-everywhere
  :commands paredit-everywhere-mode
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
  (add-hook 'css-mode-hook #'paredit-everywhere-mode))

(use-package paredit
  :diminish paredit-mode
  :commands paredit-mode
  :config
  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))


;;; Auto complete

;; not use company
(use-package company
  :disabled t
  :diminish company-mode
  :commands company-mode
  :config
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

(use-package auto-complete
  :diminish auto-complete-mode
  :commands global-auto-complete-mode
  :init
  (dolist (hook '(prog-mode-hook org-mode-hook markdown-mode-hook))
    (add-hook hook #'auto-complete-mode))
  :config
  (use-package auto-complete-config
    :ensure nil
    :demand t)
  (setq ac-auto-start 2
        ac-candidate-menu-min 0
        ac-disable-inline t
        ac-dwim t
        ac-quick-help-delay 1
        ac-quick-help-height 60
        ac-show-menu-immediately-on-auto-complete t
        ac-use-menu-map t
        ac-auto-show-menu t)
  (set-default 'ac-sources
               '(ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-semantic
                 ac-source-yasnippet))
  (ac-config-default)
  (ac-flyspell-workaround)
  (global-auto-complete-mode t)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "dict")))



(use-package eval-sexp-fu
  :commands (eval-sexp-fu-flash
             turn-on-eval-sexp-fu-flash-mode
             esf-flash-doit)
  :config
  (use-package cider-eval-sexp-fu
    :demand t
    :commands cider-esf--bounds-of-last-sexp)
  (setq eval-sexp-fu-flash-duration 0.5))

(use-package mic-paren
  :commands paren-activate
  :config
  (paren-activate))

(use-package dired-details
  :commands dired-details-install
  :init
  (setq-default dired-details-hidden-string "--- ")
  :config
  (dired-details-install))

(use-package default-text-scale
  :bind
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease))


;;; git

(use-package magit
  :bind
  ("C-x g" . magit-status))


;;; Languages


;; Lisp

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(use-package redshank
  :diminish redshank-mode
  :commands redshank-mode)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode)

(use-package lisp-mode
  :defer t
  :ensure nil
  :defines calculate-lisp-indent-last-sexp
  :preface
  (defvar lisp-mode-hook-list
    '(lisp-mode-hook
      inferior-lisp-mode-hook
      lisp-interaction-mode-hook
      slime-repl-mode-hook))

  (defvar elisp-mode-hook-list
    '(emacs-lisp-mode-hook
      inferior-emacs-lisp-mode-hook
      ielm-mode-hook))

  (defun lisp-mode-setup-hook ()
    "Lisp mode."
    (auto-fill-mode +1)
    (eldoc-mode +1)
    (flycheck-mode +1)
    (paredit-mode +1)
    (paren-activate)
    (rainbow-delimiters-mode +1)
    (redshank-mode +1)
    (turn-on-eval-sexp-fu-flash-mode)
    (aggressive-indent-mode)
    (add-hook 'after-save-hook #'check-parens nil t))

  (defun elisp-mode-setup-hook ()
    "Elisp mode."
    (elisp-slime-nav-mode +1))

  :init
  (dolist (hook lisp-mode-hook-list)
    (add-hook hook #'lisp-mode-setup-hook))

  (dolist (hook elisp-mode-hook-list)
    (add-hook hook #'lisp-mode-setup-hook)
    (add-hook hook #'elisp-mode-setup-hook)
    (add-hook hook (lambda ()
                     (setq-local lisp-indent-function
                                 #'redefine-lisp-indent-function)))))

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


;; C

(use-package google-c-style
  :commands (google-set-c-style
             google-make-newline-indent)
  :preface
  (defun c-mode-common-setup-hook ()
    (google-set-c-style)
    (google-make-newline-indent)
    (c-add-style "my-c-style"
                 '("Google"
                   (tab-width . 4)
                   (c-basic-offset . 4)
                   (indent-tabs-mode . t)
                   (c-auto-newline . t)
                   (c-electric-flag . t)))
    (c-set-style "my-c-style"))
  :init
  (add-hook 'c-mode-common-hook #'c-mode-common-setup-hook))


;; Markdown

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))


;; Clojure

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
  :init
  (add-hook 'clojure-mode-hook #'lisp-mode-setup-hook)
  (add-hook 'clojure-mode-hook #'subword-mode)
  :config
  (use-package cljsbuild-mode)
  (use-package elein))

(eval-when-compile (defun org-bookmark-jump-unhide ()))
(use-package cider
  :commands cider-mode
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'cider-mode))
  :config
  (setq nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t)
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package ac-cider
  :commands ac-cider-setup
  :init
  (with-eval-after-load 'cider-mode
    (add-hook 'cider-mode-hook #'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook #'ac-cider-setup))
  :config
  (with-eval-after-load 'auto-complete
    '(add-to-list 'ac-modes 'cider-mode)))

(use-package clj-refactor
  :commands (clj-refactor-mode
             cljr-add-keybindings-with-prefix)
  :preface
  (defun clj-refactor-setup-hook ()
    (clj-refactor-mode +1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'clj-refactor-setup-hook)))

(use-package flycheck-clojure
  :commands flycheck-clojure-setup
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'flycheck-clojure-setup)))


;; Go

(use-package go-eldoc
  :commands go-eldoc-setup
  :preface
  (defun go-eldoc-setup-hook ()
    (go-eldoc-setup))
  :init
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode-hook #'go-eldoc-setup-hook)))

(use-package go-autocomplete
  :defer t
  :init
  (with-eval-after-load 'auto-complete
    '(add-to-list 'ac-modes 'go-mode)))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :commands gofmt-before-save
  :preface
  (defun go-mode-setup-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (setq compile-command "go build -v && go test -v && go vet")
    (add-hook 'before-save-hook #'gofmt-before-save))
  :init
  (add-hook 'go-mode-hook #'go-mode-setup-hook)
  :config
  (bind-keys :map go-mode-map
    ("M-." . godef-jump)
    ("C-c C-c" . compile)))


;; Swift
(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :init
  (add-hook 'swift-mode-hook 'flycheck-mode))


;; Haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))


;; Org

(use-package org
  :ensure nil
  :defer t
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c b" . org-iswitchb)
   ("C-c k" . org-capture))
  :config
  (use-package org-capture
    :ensure nil)
  ;; Add some new modules.
  (add-to-list 'org-modules 'org-habit)

  ;; Set up paths.
  (setq org-directory "~/org"
        ;; File for capturing new tasks.
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list (concat org-directory "/todo.org")
                               org-default-notes-file))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("NEXT" :foreground "blue" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))

  (setq org-capture-templates
        (let ((refile-file (concat org-directory "/notes.org")))
          `(("t" "todo" entry (file ,refile-file)
             "* TODO %?")
            ("n" "note" entry (file ,refile-file)
             "* %?"))))

  ;; Refile setup.
  (setq org-refile-targets '((org-agenda-files :level . 1))
        org-refile-use-outline-path 'file)

  ;; Use ido completion.
  (setq org-completion-use-ido t)

  ;; Do not split line when cursor in not at the end.
  (setq org-M-RET-may-split-line nil)

  ;; Highlight source code.
  (setq org-src-fontify-natively t)

  ;; Add a timestamp when a certain TODO item was finished.
  (setq org-log-done 'time)

  ;; Custom timestamp formats.
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats
        '("<%d-%m-%Y %a>" . "<%d-%m-%Y %a %H:%M>"))

  (setq org-ellipsis "⤵")

  ;; Align org tags before saving.
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'org-align-all-tags nil t)))

  ;; Org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (python . t))))

;; LaTeX export
(use-package ox-latex
  :ensure nil
  :defer t
  :commands -repeat
  :config
  (setq org-latex-pdf-process
        (-repeat 3 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted))

;; Integration with beamer
(use-package ox-beamer
  :ensure nil
  :defer t
  :config
  ;; Don't ask me if this variable can be evaluated.
  (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv+block"
                 "O"
                 "\\begin{onlyenv}%a\\begin{block}{%h}"
                 "\\end{block}\\end{onlyenv}")))

(use-package org-journal
  :defer t
  :mode
  ("journal/[0-9]\\{8\\}$" . org-journal-mode)
  :config
  (setq org-journal-dir (concat org-directory "/journal/")))

(use-package org-bullets
  :commands org-bullets-mode
  :preface
  (defun org-bullets-mode-hook ()
    (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
    (org-bullets-mode +1))
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode-hook))


;;; key bindings

(bind-key "C-x m" #'eshell)
(bind-key "C-c r" #'revert-buffer)
(bind-key "C-c q" #'toggle-dark/light-theme)


;;; registers

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))


;;; Elapsed time

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)
;;; init.el ends here
