;;; setup-hl.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package highlight-symbol
  :diminish
  :bind
  ("C-c ." . highlight-symbol)
  :hook
  ((prog-mode html-mode css-mode) . highlight-symbol-mode)
  ((prog-mode html-mode css-mode) . highlight-symbol-nav-mode)
  :config
  (setq highlight-symbol-colors
        '("#BF616A" "#D08770" "#EBCB8B" "#A3BE8C" "#B48EAD"
          "#5E81AC" "#81A1C1" "#88C0D0" "#8FBCBB"))
  (setq highlight-symbol-foreground-color "#D8DEE9"))

(provide 'setup-hl)
;;; setup-hl.el ends here
