;;; d2-ts-mode.el --- Tree-sitter support for D2 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 the d2-ts-mode contributors

;; Author: Shota Fukumori
;; Version: 0.1.0
;; Keywords: languages d2 tree-sitter
;; URL: https://github.com/chaploud/d2-ts-mode
;; Package-Requires: ((emacs "30.1"))

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

;; A tree-sitter based major mode for editing D2 diagram files.
;; D2 is a modern diagramming language (https://d2lang.com).
;;
;; This mode provides:
;; - Syntax highlighting via tree-sitter font-lock
;; - Indentation
;; - Imenu navigation (Container, Connection, Node, Class, Table)
;; - Comment support
;; - Defun navigation (C-M-a / C-M-e)
;;
;; Requirements:
;; - Emacs 30.1 or later with tree-sitter support
;; - The D2 tree-sitter grammar (ravsii/tree-sitter-d2)
;;
;; The grammar can be installed via `M-x d2-ts-mode-reinstall-grammar'
;; or `M-x treesit-install-language-grammar'.

;;; Code:

(require 'treesit)
(require 'd2-ts-indent)
(require 'd2-ts-font-lock)
(require 'd2-ts-imenu)
(require 'd2-ts-compile)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defgroup d2-ts nil
  "Major mode for editing D2 files, powered by tree-sitter."
  :group 'languages
  :prefix "d2-ts-mode-"
  :link '(url-link :tag "GitHub" "https://github.com/sorah/d2-ts-mode")
  :link '(url-link :tag "D2 Language" "https://d2lang.com"))

(defcustom d2-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `d2-ts-mode'."
  :type 'integer
  :safe #'integerp
  :group 'd2-ts)

(defcustom d2-ts-mode-ensure-grammars t
  "When non-nil, automatically install the D2 tree-sitter grammar if missing."
  :type 'boolean
  :group 'd2-ts)

(defvar d2-ts-mode--grammar-recipe
  '(d2 "https://github.com/ravsii/tree-sitter-d2")
  "Tree-sitter grammar recipe for D2.")

(defvar d2-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Comment: # ...
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    ;; Escape
    (modify-syntax-entry ?\\ "\\" table)
    ;; Matching parens
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Punctuation
    (modify-syntax-entry ?:  "."  table)
    (modify-syntax-entry ?\; "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    table)
  "Syntax table for `d2-ts-mode'.")

(defun d2-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Extracts the first identifier from a declaration or method_declaration."
  (pcase (treesit-node-type node)
    ((or "declaration" "method_declaration")
     (let ((id (treesit-search-subtree node "identifier" nil nil 2)))
       (when id
         (treesit-node-text id t))))))

(defun d2-ts-mode--install-grammar ()
  "Install the D2 tree-sitter grammar if not already available."
  (unless (treesit-language-available-p 'd2)
    (let ((treesit-language-source-alist
           (list d2-ts-mode--grammar-recipe)))
      (treesit-install-language-grammar 'd2))))

;;;###autoload
(defun d2-ts-mode-reinstall-grammar ()
  "Reinstall the D2 tree-sitter grammar."
  (interactive)
  (let ((treesit-language-source-alist
         (list d2-ts-mode--grammar-recipe)))
    (treesit-install-language-grammar 'd2)))

(defvar-keymap d2-ts-mode-map
  :doc "Keymap for `d2-ts-mode'."
  "C-c C-c" #'d2-ts-mode-compile
  "C-c C-b" #'d2-ts-mode-compile-buffer
  "C-c C-r" #'d2-ts-mode-compile-region
  "C-c C-w" #'d2-ts-mode-watch
  "C-c C-q" #'d2-ts-mode-watch-stop
  "C-c C-p" #'d2-ts-mode-preview)

;;;###autoload
(define-derived-mode d2-ts-mode prog-mode "D2"
  "Major mode for editing D2 diagrams, powered by tree-sitter.

\\{d2-ts-mode-map}"
  :group 'd2-ts
  :syntax-table d2-ts-mode--syntax-table

  (when d2-ts-mode-ensure-grammars
    (d2-ts-mode--install-grammar))

  (when (treesit-ready-p 'd2)
    (treesit-parser-create 'd2)

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "#+\\s-*")

    ;; Electric.
    (setq-local electric-indent-chars
                (append "{}[]()" electric-indent-chars))

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("declaration" "method_declaration")))
    (setq-local treesit-defun-name-function #'d2-ts-mode--defun-name)
    (setq-local treesit-thing-settings
                `((d2
                   (defun ,(regexp-opt '("declaration"
                                         "method_declaration"))))))

    ;; Indent.
    (setq-local treesit-simple-indent-rules d2-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings d2-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                d2-ts-mode--font-lock-feature-list)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings d2-ts-mode--imenu-settings)

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'd2-ts-mode '(d2-mode))

(if (treesit-ready-p 'd2 t)
    (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-ts-mode)))

(provide 'd2-ts-mode)

;;; d2-ts-mode.el ends here
