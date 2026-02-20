;;; d2-ts-imenu.el --- Imenu support for d2-ts-mode -*- lexical-binding: t; -*-

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

;; Tree-sitter imenu navigation for D2 diagram language.

;;; Code:

(require 'treesit)

(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defun d2-ts-imenu--has-shape-p (node shape)
  "Return non-nil if declaration NODE has a `shape: SHAPE' sub-declaration."
  (let ((block (treesit-search-subtree node "block" nil nil 2)))
    (when block
      (let ((found nil)
            (i 0)
            (count (treesit-node-child-count block t)))
        (while (and (< i count) (not found))
          (let ((child (treesit-node-child block i t)))
            (when (string-equal "declaration" (treesit-node-type child))
              (let ((first-id (treesit-search-subtree child "identifier" nil nil 2)))
                (when (and first-id
                           (string-equal "shape" (treesit-node-text first-id t)))
                  (let ((label (treesit-search-subtree child "label" nil nil 2)))
                    (when (and label
                               (string-equal shape
                                             (string-trim (treesit-node-text label t))))
                      (setq found t)))))))
          (setq i (1+ i)))
        found))))

(defun d2-ts-imenu--class-p (node)
  "Return non-nil if NODE is a class declaration."
  (and (string-equal "declaration" (treesit-node-type node))
       (d2-ts-imenu--has-shape-p node "class")))

(defun d2-ts-imenu--table-p (node)
  "Return non-nil if NODE is a sql_table declaration."
  (and (string-equal "declaration" (treesit-node-type node))
       (d2-ts-imenu--has-shape-p node "sql_table")))

(defun d2-ts-imenu--container-p (node)
  "Return non-nil if NODE is a container (has block, not class/table/connection)."
  (and (string-equal "declaration" (treesit-node-type node))
       (treesit-search-subtree node "block" nil nil 2)
       (not (treesit-search-subtree node "connection" nil nil 2))
       (not (d2-ts-imenu--class-p node))
       (not (d2-ts-imenu--table-p node))))

(defun d2-ts-imenu--connection-p (node)
  "Return non-nil if NODE is a connection declaration."
  (and (string-equal "declaration" (treesit-node-type node))
       (treesit-search-subtree node "connection" nil nil 2)))

(defun d2-ts-imenu--node-p (node)
  "Return non-nil if NODE is a simple node (no block, no connection)."
  (and (string-equal "declaration" (treesit-node-type node))
       (not (treesit-search-subtree node "block" nil nil 2))
       (not (treesit-search-subtree node "connection" nil nil 2))))

(defvar d2-ts-mode--imenu-settings
  `(("Class" "\\`declaration\\'" d2-ts-imenu--class-p nil)
    ("Table" "\\`declaration\\'" d2-ts-imenu--table-p nil)
    ("Container" "\\`declaration\\'" d2-ts-imenu--container-p nil)
    ("Connection" "\\`declaration\\'" d2-ts-imenu--connection-p nil)
    ("Node" "\\`declaration\\'" d2-ts-imenu--node-p nil))
  "Imenu settings for `d2-ts-mode'.")

(provide 'd2-ts-imenu)

;;; d2-ts-imenu.el ends here
