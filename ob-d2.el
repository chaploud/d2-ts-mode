;;; ob-d2.el --- Org-Babel support for D2 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 the d2-ts-mode contributors

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating D2 diagram source blocks.
;;
;; Usage in Org files:
;;
;;   #+begin_src d2 :file diagram.svg
;;   x -> y -> z
;;   #+end_src
;;
;; The `:file' header argument is required.
;; When `d2-ts-mode' is loaded, `d2-ts-mode-d2-executable' and
;; `d2-ts-mode-compile-flags' are respected.

;;; Code:

(require 'ob)
(require 'ob-eval)

(defvar d2-ts-mode-d2-executable)
(defvar d2-ts-mode-compile-flags)

(defvar org-babel-default-header-args:d2
  '((:results . "file") (:exports . "results"))
  "Default header arguments for D2 source blocks.")

(defun org-babel-execute:d2 (body params)
  "Execute a D2 source block BODY with PARAMS.
Compile the block to an image file specified by the `:file' header."
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "D2 source block requires a \":file\" header argument")))
         (d2-bin (if (boundp 'd2-ts-mode-d2-executable)
                     d2-ts-mode-d2-executable
                   "d2"))
         (flags (and (boundp 'd2-ts-mode-compile-flags)
                     d2-ts-mode-compile-flags))
         (temp-file (org-babel-temp-file "d2-" ".d2"))
         (cmd (mapconcat #'shell-quote-argument
                         (append (list d2-bin
                                       temp-file
                                       (org-babel-process-file-name out-file))
                                 flags)
                         " ")))
    (with-temp-file temp-file (insert body))
    (org-babel-eval cmd "")
    nil))

(provide 'ob-d2)

;;; ob-d2.el ends here
