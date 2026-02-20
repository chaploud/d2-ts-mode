;;; d2-ts-font-lock.el --- Font-lock rules for d2-ts-mode -*- lexical-binding: t; -*-

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

;; Tree-sitter font-lock rules for D2 diagram language.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(defvar d2-ts-mode--builtin-identifiers
  '("3d"
    "animated"
    "bold"
    "border-radius"
    "class"
    "classes"
    "constraint"
    "d2-config"
    "d2-legend"
    "direction"
    "double-border"
    "fill"
    "fill-pattern"
    "filled"
    "font"
    "font-color"
    "font-size"
    "height"
    "icon"
    "italic"
    "label"
    "layers"
    "level"
    "link"
    "multiple"
    "near"
    "opacity"
    "scenarios"
    "shadow"
    "shape"
    "source-arrowhead"
    "steps"
    "stroke"
    "stroke-dash"
    "stroke-width"
    "style"
    "target-arrowhead"
    "text-transform"
    "tooltip"
    "top"
    "underline"
    "vars"
    "width")
  "D2 built-in identifier names for font-locking.")

(defvar d2-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Level 1: comment, definition
   :language 'd2
   :feature 'comment
   '([(comment) (block_comment)] @font-lock-comment-face)

   :language 'd2
   :feature 'definition
   '((declaration :anchor (identifier) @font-lock-function-name-face)
     (declaration :anchor (identifier_chain :anchor (identifier) @font-lock-function-name-face))
     (argument_name) @font-lock-variable-name-face
     (argument_type) @font-lock-type-face)

   ;; Level 2: keyword, string, builtin
   :language 'd2
   :feature 'keyword
   '(["$" "...$" "@" "...@"] @font-lock-keyword-face
     [(glob_filter) (inverse_glob_filter) (visibility_mark)]
     @font-lock-keyword-face
     ((identifier) @font-lock-keyword-face
      (:match "\\`_\\'" @font-lock-keyword-face))
     )

   :language 'd2
   :feature 'string
   '([(label) (label_codeblock)] @font-lock-string-face)

   :language 'd2
   :feature 'builtin
   :override t
   `(((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(seq bos
                      (or ,@d2-ts-mode--builtin-identifiers)
                      eos))
              @font-lock-builtin-face)))

   ;; Level 3: constant, number, escape-sequence, variable, import
   :language 'd2
   :feature 'constant
   '((boolean) @font-lock-constant-face)

   :language 'd2
   :feature 'number
   '([(integer) (float)] @font-lock-number-face)

   :language 'd2
   :feature 'escape-sequence
   :override t
   '((escape) @font-lock-escape-face)

   :language 'd2
   :feature 'variable
   '([(variable) (spread_variable)] @font-lock-variable-use-face)

   :language 'd2
   :feature 'import
   '((import) @font-lock-keyword-face)

   ;; Level 4: bracket, delimiter, operator, glob, error
   :language 'd2
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"
      "|" "||" "|||" "|`" "`|"])
     @font-lock-bracket-face)

   :language 'd2
   :feature 'delimiter
   '((["." "," ":" ";"]) @font-lock-delimiter-face)

   :language 'd2
   :feature 'operator
   '((connection) @font-lock-operator-face)

   :language 'd2
   :feature 'glob
   '([(glob) (recursive_glob) (global_glob)] @font-lock-regexp-face)

   :language 'd2
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `d2-ts-mode'.")

(defvar d2-ts-mode--font-lock-feature-list
  '((comment definition)
    (keyword string builtin)
    (constant number escape-sequence variable import)
    (bracket delimiter operator glob error))
  "Font-lock feature list for `d2-ts-mode'.")

(provide 'd2-ts-font-lock)

;;; d2-ts-font-lock.el ends here
