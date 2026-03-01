#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

EL_FILES=(
  d2-ts-indent.el
  d2-ts-font-lock.el
  d2-ts-imenu.el
  d2-ts-compile.el
  d2-ts-mode.el
)

pass=0
fail=0

run_check() {
  local name=$1
  shift
  printf "\033[1m[%s]\033[0m " "$name"
  if output=$("$@" 2>&1); then
    printf "\033[32mOK\033[0m\n"
    pass=$((pass + 1))
  else
    printf "\033[31mFAIL\033[0m\n"
    echo "$output" | sed 's/^/  /'
    fail=$((fail + 1))
  fi
}

# byte-compile
run_check "byte-compile" emacs --batch -L . \
  --eval "(setq byte-compile-error-on-warn t)" \
  -f batch-byte-compile "${EL_FILES[@]}"

# package-lint
run_check "package-lint" emacs --batch -L . \
  --eval "(require 'package)" \
  --eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
  --eval "(package-initialize)" \
  --eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
  --eval "(require 'package-lint)" \
  -f package-lint-batch-and-exit \
  d2-ts-mode.el

# checkdoc
files_lisp=$(printf '"%s" ' "${EL_FILES[@]}")
run_check "checkdoc" emacs --batch \
  --eval "(require 'checkdoc)" \
  --eval "
(let ((files '(${files_lisp}))
      (found 0))
  (advice-add 'display-warning :before
              (lambda (&rest _) (setq found (1+ found))))
  (dolist (file files)
    (checkdoc-file file))
  (when (> found 0)
    (kill-emacs 1)))"

# summary
echo ""
printf "\033[1m%d passed, %d failed\033[0m\n" "$pass" "$fail"
exit "$fail"
