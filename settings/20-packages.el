;;; 20-packages.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq package-enable-at-startup t)

(provide '20-packages)
;;; 20-packages.el ends here
