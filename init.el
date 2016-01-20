;;; init.el --- youngker's configuration

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Keywords: configuration
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
(deftheme default-black
  "Default-black theme.")

(custom-theme-set-faces
 'default-black
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(region ((nil (:background "#464740"))))
 '(hl-line ((nil (:background "#222222"))))
 '(yas-field-highlight-face ((nil (:background "#333399"))))
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red"))))
 '(eval-sexp-fu-flash ((t (:background "#333399" :foreground "White"))))
 '(helm-source-header ((t (:background "Black" :foreground "Orange" :bold nil))))
 '(helm-candidate-number ((t (:background nil :foreground "#87cefa" :bold t))))
 '(helm-selection ((t (:background "#222222"))))
 '(helm-visible-mark ((t (:background "#444444" :forground "White"))))
 '(helm-match ((t (:background "Black" :foreground "#008b8b"))))
 '(ac-candidate-face ((t (:background "#1a1a1a" :foreground "#a6a6a6"))))
 '(ac-selection-face ((t (:background "#2e2e2e" :foreground "#a6a6a6"))))
 '(popup-isearch-match ((t (:background "Black" :foreground "deep pink"))))
 '(popup-tip-face ((t (:background "#1a1a1a" :foreground "#a6a6a6"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#6e6e6e"))))
 '(popup-scroll-bar-background-face ((t (:background "#383838")))))

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default
 auto-revert-verbose nil
 blink-matching-paren nil ;;
 column-number-mode t
 confirm-nonexistent-file-or-buffer nil
 delete-by-moving-to-trash nil
 delete-selection-mode t
 echo-keystrokes 0.02
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 electric-indent-mode nil
 fill-column 80
 gc-cons-threshold 20000000
 global-auto-revert-non-file-buffers t
 history-length 1000
 indent-tabs-mode nil
 inhibit-startup-message t
 initial-major-mode 'text-mode
 make-backup-files nil
 next-error-highlight t
 next-error-highlight-no-select t
 query-replace-highlight t
 recentf-max-saved-items 100
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

(auto-compression-mode 1)
(blink-cursor-mode -1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(winner-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; font
(cond
 ((eq window-system 'x)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 10"))
 ((eq window-system 'w32)
  (set-face-attribute 'default nil :font "Consolas bold 11"))
 ((memq window-system '(ns mac))
  (set-face-attribute 'default nil :font "Monaco 13")))

;; locale
(set-language-environment "Korean")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;; Bootstrap `use-package'

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize 'noactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish nil t)
(setq use-package-verbose t)
(setq use-package-always-ensure t)


;;;

(use-package auto-compile
  :after ido
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package page-break-lines
  :after ido
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode)
  (turn-on-page-break-lines-mode))

(use-package whitespace-cleanup-mode
  :after ido
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(use-package smooth-scrolling
  :disabled t)

(use-package subword
  :diminish subword-mode
  :after ido
  :config
  (global-subword-mode 1))

(use-package recentf
  :after ido
  :config
  (recentf-mode 1))

(use-package saveplace
  :after ido
  :config
  (setq-default save-place t
                save-place-file (concat user-emacs-directory "place")))

;;; Mac

(when (equal system-type 'darwin)
  (use-package exec-path-from-shell
    :after ido
    :config
    (exec-path-from-shell-initialize))

  (menu-bar-mode +1)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))


;;; Ido

(defun youngker/ido-recent-file ()
  "Find a recent file."
  (interactive)
  (let ((file (ido-completing-read
               "Choose recent file: "
               (mapcar 'abbreviate-file-name recentf-list) nil t)))
    (when file
      (find-file file))))

(use-package ido
  :bind
  (("C-x C-f" . ido-find-file)
   ("C-x f"   . youngker/ido-recent-file)
   ("C-x b"   . ido-switch-buffer)
   ("C-x B"   . ido-switch-buffer-other-window))
  :config
  (use-package flx-ido
    :config
    (flx-ido-mode t))

  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only))

  (ido-mode t)
  (icomplete-mode 1)
  (setq ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-case-fold nil
        ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-everywhere 1
        ido-max-prospects 10
        ido-use-faces nil
        ido-use-filename-at-point nil
        ido-enable-prefix nil)
  (define-key ido-file-completion-map (kbd "C-\\") 'backward-kill-word))


;;; helm

(use-package helm
  :bind
  (("C-x C-i" . helm-imenu)
   ("M-y"     . helm-show-kill-ring)
   ("C-c h w" . helm-descbinds)
   ("C-c h f" . helm-codesearch-find-file)
   ("C-c h t" . helm-codesearch-find-pattern)
   ("C-c h I" . helm-codesearch-create-csearchindex)
   ("M-i"     . helm-swoop)
   ("M-I"     . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (use-package helm-config :ensure nil)
  (use-package helm-command :ensure nil)
  (use-package helm-semantic :ensure nil)
  (use-package helm-codesearch)
  (use-package helm-descbinds)
  (use-package helm-swoop)
  (unbind-key "C-x c")
  (bind-keys* :prefix-map helm-command-prefix
              :prefix "C-c h"
              ("w" . helm-descbinds)
              ("f" . helm-codesearch-find-file)
              ("t" . helm-codesearch-find-pattern)
              ("I" . helm-codesearch-create-csearchindex))

  (bind-keys*
   ("M-i"     . helm-swoop)
   ("M-I"     . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))

  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-i"   . helm-execute-persistent-action)
             ("C-z"   . helm-select-action))

  (helm-autoresize-mode 1)
  (setq
   helm-M-x-fuzzy-match        t
   helm-M-x-requires-pattern   nil
   helm-autoresize-max-height  30
   helm-autoresize-min-height 30
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
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))

(use-package avy
  :bind ("C-;" . avy-goto-char)
  :config
  (avy-setup-default))

(use-package volatile-highlights
  :after ido
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package win-switch
  :bind ("C-x o" . win-switch-dispatch)
  :config
  (setq win-switch-feedback-background-color "DeepPink3")
  (setq win-switch-feedback-foreground-color "black")
  (setq win-switch-window-threshold 1)
  (setq win-switch-idle-time 0.7)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window))

(use-package popwin
  :after ido
  :config
  (popwin-mode 1))

(use-package undo-tree
  :bind
  ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package yasnippet
  :after ido
  :diminish yas-minor-mode
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (yas-global-mode 1))

(use-package aggressive-indent
  :after ido
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package which-key
  :diminish which-key-mode
  :defer 5
  :config
  (which-key-mode 1))

(use-package visual-regexp
  :bind
  ("M-/" . vr/replace))

(use-package highlight-symbol
  :bind
  (("C-<f3>" . highlight-symbol)
   ("S-<f3>" . highlight-symbol-prev)
   ("M-<f3>" . highlight-symbol-next)))

(use-package eopengrok
  :bind
  (("C-c s I" . eopengrok-make-index)
   ("C-c s d" . eopengrok-find-definition)
   ("C-c s f" . eopengrok-find-file)
   ("C-c s s" . eopengrok-find-reference)
   ("C-c s t" . eopengrok-find-text)
   ("C-c s h" . eopengrok-find-history)
   ("C-c s b" . eopengrok-resume))
  :init
  (setq eopengrok-jar   "/Users/youngker/Projects/opengrok-0.12.1.5/lib/opengrok.jar")
  (setq eopengrok-ctags "/usr/local/bin/ctags"))

(use-package google-translate
  :bind ("C-c t" . google-translate-smooth-translate)
  :config
  (use-package google-translate-smooth-ui :ensure nil)
  ;; (setq google-translate-output-destination 'popup)
  (setq google-translate-translation-directions-alist
        '(("en" . "ko") ("ko" . "en"))))


;;; Syntax check

(use-package flycheck
  :commands flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon
    'flycheck-show-error-at-point))


;;; Lisp tools

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package paredit-everywhere
  :commands paredit-everywhere-mode
  :init
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
  (add-hook 'css-mode-hook 'paredit-everywhere-mode))

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
  :commands ac-emacs-lisp-mode-setup
  :config
  (use-package auto-complete-config :ensure nil)
  (setq ac-auto-show-menu t)
  (setq ac-dwim t)
  (setq ac-use-menu-map t)
  (setq ac-quick-help-delay 1)
  (setq ac-quick-help-height 60)
  (setq ac-disable-inline t)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (setq ac-auto-start 2)
  (setq ac-candidate-menu-min 0)
  (set-default 'ac-sources
               '(ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-semantic
                 ac-source-yasnippet))
  (ac-config-default)
  (ac-flyspell-workaround)
  (global-auto-complete-mode t)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "dict"))
  (dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                      sass-mode yaml-mode csv-mode espresso-mode
                                      haskell-mode html-mode nxml-mode sh-mode
                                      smarty-mode clojure-mode lisp-mode
                                      textile-mode markdown-mode tuareg-mode))
    (add-to-list 'ac-modes mode))
  ;;Key triggers
  (define-key ac-completing-map (kbd "C-M-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map (kbd "M-RET") 'ac-help)
  (define-key ac-completing-map "\r" 'nil))




(use-package eval-sexp-fu
  :commands turn-on-eval-sexp-fu-flash-mode
  :config
  (use-package cider-eval-sexp-fu)
  (setq eval-sexp-fu-flash-duration 0.5))

(use-package mic-paren
  :after ido
  :config
  (paren-activate))

;;; git

(use-package magit
  :bind
  ("C-x g" . magit-status))


;;; Languages

(defsubst youngker/add-hook (func &rest hook)
  "Add hook with FUNC and HOOK."
  (dolist (mode-hook hook)
    (add-hook mode-hook func)))


;; Lisp
(defun youngker/lisp-mode ()
  "Lisp mode."
  (auto-fill-mode 1)
  (eldoc-mode 1)
  (flycheck-mode 1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (redshank-mode 1)
  (turn-on-eval-sexp-fu-flash-mode)
  (add-hook 'after-save-hook 'check-parens nil t))

(defun youngker/elisp-mode ()
  "Elisp mode."
  (ac-emacs-lisp-mode-setup)
  (elisp-slime-nav-mode 1))

(use-package redshank
  :diminish redshank-mode
  :commands redshank-mode)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode)

(use-package lisp-mode
  :defer t
  :ensure nil
  :init
  (apply #'youngker/add-hook #'youngker/lisp-mode
         '(emacs-lisp-mode-hook
           inferior-emacs-lisp-mode-hook
           ielm-mode-hook
           lisp-mode-hook
           inferior-lisp-mode-hook
           lisp-interaction-mode-hook
           slime-repl-mode-hook))
  (apply #'youngker/add-hook #'youngker/elisp-mode
         '(emacs-lisp-mode-hook
           inferior-emacs-lisp-mode-hook
           ielm-mode-hook)))


;; C-c++

(use-package google-c-style
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.c\\'"                   . c-mode)
         ("\\.cc\\'"                  . c++-mode))
  :config
  (youngker/add-hook #'google-set-c-style 'c-mode-common-hook)
  (youngker/add-hook #'google-make-newline-indent 'c-mode-common-hook))


;; Markdown

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))


;; Clojure

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
  :init
  (youngker/add-hook #'youngker/lisp-mode 'clojure-mode-hook)
  (youngker/add-hook #'subword-mode 'clojure-mode-hook)
  ;; (youngker/add-hook (lambda ()
  ;;                      (flycheck-clojure-setup)) 'clojure-mode-hook)
  :config
  (use-package cljsbuild-mode)
  (use-package elein))

(use-package cider
  :commands (cider-jack-in cider-connect)
  :bind
  ("C-c M-j" . cider-jack-in))

(use-package ac-cider
  :commands ac-cider-setup
  :init
  (youngker/add-hook (lambda ()
                       (ac-cider-setup))
                     'cider-mode-hook
                     'cider-repl-mode-hook)
  :config
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-mode)))

(use-package clj-refactor
  :commands clj-refactor-mode
  :init
  (youngker/add-hook (lambda ()
                       (clj-refactor-mode 1)
                       (cljr-add-keybindings-with-prefix "C-c C-m"))
                     'clojure-mode-hook)
  :config
  (bind-key "C->" #'cljr-cycle-coll clojure-mode-map))

(use-package flycheck-clojure
  :commands flycheck-clojure-setup)


;; Go

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (bind-keys :map go-mode-map
             ("M-." 'godef-jump)))


;;; Elapsed time

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)
;;; init.el ends here
