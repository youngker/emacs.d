;;; consult-codesearch.el --- consult interface for codesearch -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "27.1") (codesearch "1") (consult "0.20") (embark 0.18))

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

(require 'codesearch)
(require 'consult)
(require 'embark-consult)

(defgroup consult-codesearch nil
  "Consult interface for codesearch."
  :prefix "consult-codesearch-"
  :group 'consult)

(defvar consult-codesearch-args nil
  "Codesearch arguments.")

(defcustom consult-codesearch-file
  "csearch -l -f"
  "Codesearch file search command."
  :type 'string
  :group 'consult-codesearch)

(defcustom consult-codesearch-pattern
  "csearch -n"
  "Codesearch search command."
  :type 'string
  :group 'consult-codesearch)

(defconst consult-codesearch--match-regexp
  "\\`\\(?:\\./\\)?\\([^\n\0]+\\):\\([0-9]+\\)\\([-:\0]\\)"
  "Regexp used to match file and line of codesearch output.")

(defun consult-codesearch--builder (input)
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

(defun consult-codesearch--set-index (dir)
  "Set CSEARCHINDEX variable in DIR."
  (let* ((search-dir (or dir default-directory))
         (index-file (codesearch--csearchindex search-dir)))
    (setenv "CSEARCHINDEX" index-file)))

;;;###autoload
(defun consult-codesearch-build-index (dir)
  "Create index file at DIR."
  (interactive "DIndex files in directory: ")
  (let ((index-file (concat dir codesearch-csearchindex)))
    (codesearch-build-index (expand-file-name dir) index-file)))

;;;###autoload
(defun consult-codesearch (&optional dir initial)
  "Call the \"csearch\" shell command."
  (interactive "P")
  (let ((initial (thing-at-point 'symbol))
        (consult-codesearch-args consult-codesearch-pattern)
        (consult--grep-match-regexp consult-codesearch--match-regexp)
        (index (consult-codesearch--set-index dir)))
    (consult--grep "Codesearch" #'consult-codesearch--builder dir initial)))

;;;###autoload
(defun consult-codesearch-file (&optional dir initial)
  "Call the \"csearch\" shell command for find file."
  (interactive "P")
  (let* ((initial (thing-at-point 'symbol))
         (consult-codesearch-args consult-codesearch-file)
         (index (consult-codesearch--set-index dir))
         (prompt-dir (consult--directory-prompt "Codesearch Find" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir)
                              #'consult-codesearch--builder initial))))

(provide 'consult-codesearch)
;;; consult-codesearch.el ends here
