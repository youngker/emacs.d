;;; setup-clojure.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
  :defines
  (inferior-lisp-program)
  :config
  (setq inferior-lisp-program "lein repl"))

(use-package inf-clojure
  :disabled t
  :hook
  (clojure-mode . inf-clojure-minor-mode))

(provide 'setup-clojure)
;;; setup-clojure.el ends here
