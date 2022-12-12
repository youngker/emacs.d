;;; consult-codesearch.el --- consult interface for codesearch -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.2") (consult "1.7.7"))

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

;; consult interface for codesearch
;;
;; See documentation on https://github.com/youngker/consult-codesearch.el

;;; Code:

(require 'consult)

(defcustom consult-codesearch-index ".csearchindex"
  "Index file for each projects."
  :type 'string
  :group 'consult-codesearch)

(defvar consult-codesearch-indexing-buffer "*consult codesearch indexing*")

(defvar consult-codesearch-args nil)

(defcustom consult-codesearch-file
  "csearch -l -f"
  "Find File.")

(defcustom consult-codesearch-pattern
  "csearch -n"
  "Codesearch.")

(defconst consult-codesearch--match-regexp
  "\\`\\(?:\\./\\)?\\([^\n\0]+\\):\\([0-9]+\\)\\([-:\0]\\)"
  "Regexp used to match file and line of codesearch output.")

(defun consult-codesearch-builder (input)
  "Build command line given INPUT."
  (pcase-let* ((cmd (consult--build-args consult-codesearch-args))
               (`(,arg . ,opts) (consult--command-split input))
               (flags (append cmd opts))
               (ignore-case (member "-i" flags))
               (file (member "-l" cmd))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended ignore-case)))
    (when re
      `(:command (,@cmd
                  ,@opts
                  ,(if file
                       (concat "(?i)" (consult--join-regexps re 'extended))
                     (consult--join-regexps re 'extended))
                  ,@(and file '("$")))
        :highlight ,hl))))

(defun consult-codesearch--set-index ()
  (setenv "CSEARCHINDEX"
          (expand-file-name
           (let* ((start-dir (expand-file-name default-directory))
                  (index-dir (locate-dominating-file start-dir
                                                     consult-codesearch-index)))
             (if index-dir
                 (concat index-dir consult-codesearch-index)
               (error "Can't find csearchindex"))))))

;;;###autoload
(defun consult-codesearch-create-index (dir)
  (interactive "DIndex files in directory: ")
  (setenv "CSEARCHINDEX"
          (expand-file-name (concat dir consult-codesearch-index)))

  (let* ((buf consult-codesearch-indexing-buffer)
         (proc (apply 'start-process "codesearch"
                      buf "cindex" (list (expand-file-name dir)))))
    (set-process-filter proc
                        (lambda (process output)
                          (with-current-buffer (process-buffer process)
                            (let ((buffer-read-only nil))
                              (insert output)))))
    (set-process-sentinel proc
                          (lambda (process event)
                            (with-current-buffer (process-buffer process)
                              (when (string= event "finished\n")
                                (let ((buffer-read-only nil))
                                  (insert "\nIndexing finished"))))))
    (with-current-buffer buf
      (local-set-key (kbd "q") 'quit-window)
      (let ((buffer-read-only nil))
        (erase-buffer))
      (setq buffer-read-only t)
      (pop-to-buffer buf))))

;;;###autoload
(defun consult-codesearch (&optional dir initial)
  (interactive "P")
  (let ((initial (thing-at-point 'symbol))
        (consult-codesearch-args consult-codesearch-pattern)
        (consult--grep-match-regexp consult-codesearch--match-regexp)
        (idx-path (consult-codesearch--set-index)))
    (consult--grep "Codesearch" #'consult-codesearch-builder dir initial)))

;;;###autoload
(defun consult-codesearch-file (&optional dir initial)
  (interactive "P")
  (let* ((initial (thing-at-point 'symbol))
         (consult-codesearch-args consult-codesearch-file)
         (idx-path (consult-codesearch--set-index))
         (prompt-dir (consult--directory-prompt "Find" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir)
                              #'consult-codesearch-builder initial))))

(provide 'consult-codesearch)
;;; consult-codesearch.el ends here
