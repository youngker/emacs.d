;;; init.el --- youngker's configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))

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

(let ((dir (expand-file-name "settings" user-emacs-directory)))
  (dolist (file (directory-files dir t "\\.el$"))
    (load file)))

(provide 'init)
;;; init.el ends here
