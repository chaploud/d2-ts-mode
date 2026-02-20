;;; d2-ts-indent.el --- Indentation rules for d2-ts-mode -*- lexical-binding: t; -*-

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

;; Tree-sitter indentation rules for D2 diagram language.

;;; Code:

(require 'treesit)

(defvar d2-ts-mode-indent-offset)

(defvar d2-ts-mode--indent-rules
  `((d2
     ((parent-is "source_file") column-0 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "|") parent-bol 0)
     ((node-is "||") parent-bol 0)
     ((node-is "|||") parent-bol 0)
     ((node-is "`|") parent-bol 0)
     ((parent-is "block") parent-bol d2-ts-mode-indent-offset)
     ((parent-is "label_codeblock") no-indent 0)
     ((parent-is "block_comment") no-indent 0)
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `d2-ts-mode'.")

(provide 'd2-ts-indent)

;;; d2-ts-indent.el ends here
