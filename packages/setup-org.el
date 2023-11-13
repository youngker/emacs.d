;;; setup-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :commands org-babel-do-load-languages
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o l" . org-store-link)
   ("C-c o b" . org-iswitchb)
   ("C-c o k" . org-capture))
  :config
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list (concat org-directory "/todo.org")
                               org-default-notes-file)
        org-M-RET-may-split-line nil
        org-src-fontify-natively t
        org-log-done 'time
        org-ellipsis "..."
        org-confirm-babel-evaluate nil
        org-display-custom-times t
        org-time-stamp-custom-formats
        '("<%d-%m-%Y %a>" . "<%d-%m-%Y %a %H:%M>")
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(c)"))
        org-structure-template-alist
        (eval (car (get 'org-structure-template-alist 'standard-value))))

  (add-to-list 'org-modules 'org-habit)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (dot . t)
     (plantuml . t)))

  (use-package org-capture
    :ensure nil
    :config
    (setq org-capture-templates
          (let ((refile-file (concat org-directory "/notes.org")))
            `(("t" "todo" entry (file ,refile-file)
               "* TODO %?")
              ("n" "note" entry (file ,refile-file)
               "* %?")))))

  (use-package ob-plantuml
    :ensure nil
    :init
    (setq org-plantuml-executable-path "plantuml"
          org-plantuml-exec-mode 'plantuml))

  (use-package engrave-faces)

  (use-package org-faces
    :ensure nil
    :config
    (setq org-todo-keyword-faces
          '(("NEXT" :foreground "#5E81AC" :weight bold)
            ("WAITING" :foreground "#D08770" :weight bold)
            ("HOLD" :foreground "#BF616A" :weight bold)
            ("CANCELLED" :foreground "#A3BE8C" :weight bold))))

  ;; Refile setup.
  (use-package org-refile
    :ensure nil
    :config
    (setq org-refile-targets '((org-agenda-files :level . 1))
          org-refile-use-outline-path 'file))

  (use-package org-bullets
    :defines
    (org-bullets-bullet-list)
    :functions
    (org-bullets-mode)
    :preface
    (defun org-bullets-mode-hook ()
      (org-bullets-mode))
    :hook
    (org-mode . org-bullets-mode-hook)
    :config
    (setq org-bullets-bullet-list '("•")))

  (use-package org-journal
    :mode
    ("journal/[0-9]\\{8\\}$" . org-journal-mode)
    :defines
    (org-journal-dir)
    :config
    (setq org-journal-dir (concat org-directory "/journal/")))

  (use-package ox-beamer
    :ensure nil
    :config
    (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)
    (add-to-list 'org-beamer-environments-extra
                 '("onlyenv+block"
                   "O"
                   "\\begin{onlyenv}%a\\begin{block}{%h}"
                   "\\end{block}\\end{onlyenv}")))

  (use-package ox-latex
    :ensure nil
    :init
    (setq org-latex-compiler "lualatex"
          org-latex-src-block-backend 'engraved
          org-latex-pdf-process
          (let
              ((cmd (concat "lualatex -shell-escape -interaction nonstopmode"
                            " --synctex=1"
                            " -output-directory %o %f")))
            (list cmd
                  "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
                  "cd %o; bibtex %b"
                  cmd
                  cmd)))
    :config
    (add-to-list 'org-latex-classes
                 '("extarticle"
                   "\\documentclass{extarticle}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("scrartcl" "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (use-package ox-pandoc
    :defines
    (org-pandoc-menu-entry)
    :init
    (setq org-pandoc-menu-entry
          '((?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
            (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
            (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
            (?G "as gfm." org-pandoc-export-as-gfm)
            (?j "to json and open." org-pandoc-export-to-json-and-open)
            (?J "as json." org-pandoc-export-as-json)
            (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
            (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
            (?n "to native and open." org-pandoc-export-to-native-and-open)
            (?N "as native." org-pandoc-export-as-native)
            (?o "to odt and open." org-pandoc-export-to-odt-and-open)
            (?O "to odt." org-pandoc-export-to-odt)
            (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
            (?P "to pptx." org-pandoc-export-to-pptx)
            (?t "to tex and open." org-pandoc-export-to-latex-and-open)
            (?T "to tex." org-pandoc-export-to-latex)
            (?x "to docx and open." org-pandoc-export-to-docx-and-open)
            (?X "to docx." org-pandoc-export-to-docx))))

  (use-package ox-reveal
    :defines
    (org-reveal-mathjax
     org-reveal-root)
    :init
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
    (setq org-reveal-mathjax t))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'org-align-all-tags nil t))))

(provide 'setup-org)
;;; setup-org.el ends here
