# d2-ts-mode

A tree-sitter based major mode for editing [D2](https://d2lang.com) diagram files in Emacs 30+.

## Features

- **Syntax highlighting** via tree-sitter font-lock (4 levels of detail)
- **Indentation** for blocks, labels, and code blocks
- **Imenu navigation** with categories: Class, Table, Container, Connection, Node
- **Comment support** (`M-;`, comment/uncomment region)
- **Defun navigation** (`C-M-a` / `C-M-e` between top-level declarations)
- **Compile** current file or buffer to SVG/PNG/PDF (`C-c C-c`, `C-c C-b`)
- **Watch mode** with live browser preview via `d2 --watch` (`C-c C-w`)
- **Emacs image preview** with auto-revert on save (`C-c C-p`)
- **Compatibility** with existing `d2-mode` via `derived-mode-add-parents`

## Requirements

- Emacs 30.1 or later with tree-sitter support
- The D2 tree-sitter grammar ([ravsii/tree-sitter-d2](https://github.com/ravsii/tree-sitter-d2))
- A C compiler for building the grammar (cc/gcc/clang)
- [d2 CLI](https://d2lang.com/tour/install) (for compile/watch/preview commands)

## Installation

### MELPA (recommended)

Once available on MELPA:

```elisp
(use-package d2-ts-mode
  :ensure t)
```

### Manual

Clone this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/d2-ts-mode")
(require 'd2-ts-mode)
```

### straight.el

```elisp
(use-package d2-ts-mode
  :straight (:host github :repo "sorah/d2-ts-mode"))
```

## Grammar Setup

The grammar is automatically installed when you first open a `.d2` file
(controlled by `d2-ts-mode-ensure-grammars`, default `t`).

To manually install or reinstall the grammar:

```
M-x d2-ts-mode-reinstall-grammar
```

Or via the built-in tree-sitter command:

```
M-x treesit-install-language-grammar RET d2 RET
```

When prompted for the repository URL, use: `https://github.com/ravsii/tree-sitter-d2`

## Key Bindings

| Key       | Command                    | Description                          |
|-----------|----------------------------|--------------------------------------|
| `C-c C-c` | `d2-ts-mode-compile`       | Compile current file                 |
| `C-c C-b` | `d2-ts-mode-compile-buffer`| Compile buffer (works unsaved)       |
| `C-c C-w` | `d2-ts-mode-watch`         | Start live preview (browser or Emacs)|
| `C-c C-q` | `d2-ts-mode-watch-stop`    | Stop live preview                    |
| `C-c C-p` | `d2-ts-mode-preview`       | Toggle Emacs image preview           |

## Customization

```elisp
(use-package d2-ts-mode
  :ensure t
  :custom
  ;; Number of spaces per indentation level
  (d2-ts-mode-indent-offset 2)
  ;; Auto-install tree-sitter grammar if missing
  (d2-ts-mode-ensure-grammars t)
  ;; Path to d2 binary (or absolute path like "/usr/local/bin/d2")
  (d2-ts-mode-d2-executable "d2")
  ;; Output format: "svg", "png", or "pdf"
  (d2-ts-mode-output-format "svg")
  ;; Extra CLI flags passed to d2 (e.g. '("--theme" "200" "--dark-theme" "200"))
  (d2-ts-mode-compile-flags nil)
  ;; Where C-c C-w shows live preview: 'emacs (side window) or 'browser (d2 default)
  (d2-ts-mode-watch-method 'emacs))
```

| Variable                      | Default     | Description                                          |
|-------------------------------|-------------|------------------------------------------------------|
| `d2-ts-mode-indent-offset`    | `2`         | Number of spaces per indentation level               |
| `d2-ts-mode-ensure-grammars`  | `t`         | Auto-install grammar if missing                      |
| `d2-ts-mode-d2-executable`    | `"d2"`      | Path to the d2 binary                                |
| `d2-ts-mode-output-format`    | `"svg"`     | Output format: `"svg"`, `"png"`, or `"pdf"`          |
| `d2-ts-mode-compile-flags`    | `nil`       | Extra CLI flags passed to d2 (list of strings)       |
| `d2-ts-mode-watch-method`     | `'emacs`    | `'emacs` or `'browser` — where `C-c C-w` previews   |

### Font-lock levels

Adjust the level of syntax highlighting with `treesit-font-lock-level`:

- **Level 1**: Comments, definitions
- **Level 2**: Keywords, strings, builtins (default)
- **Level 3**: Constants, numbers, escape sequences, variables, imports
- **Level 4**: Brackets, delimiters, operators, globs, errors

## Comparison with d2-mode

| Feature              | d2-mode (regex)     | d2-ts-mode (tree-sitter) |
|----------------------|---------------------|--------------------------|
| Syntax highlighting  | Basic regex patterns| Full grammar-based       |
| Indentation          | Heuristic-based     | AST-based                |
| Imenu                | Not supported       | Categorized navigation   |
| Defun navigation     | Not supported       | C-M-a / C-M-e           |
| Compile / Preview    | Supported           | Supported                |
| Emacs version        | 26.1+               | 30.1+                    |

## License

GPL-3.0-or-later
