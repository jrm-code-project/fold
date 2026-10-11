# Copilot Instructions

Small Common Lisp library (ASDF system `fold`, loaded via Quicklisp) providing `FOLD-LEFT` / `FOLD-RIGHT` and variants for alists, plists and hash tables.

## Build / test / lint

- No build system, linter, or test framework. Load with `(ql:quickload "fold")` (repo must live in `~/quicklisp/local-projects/fold/`) or `(asdf:load-system "fold")`.
- Test suite: `fold-tests.asd` + `tests.lisp` (package `FOLD-TESTS`, dependency-free `deftest`/`check` macros). Run all with `(asdf:test-system "fold-tests")`; run one with `(fold-tests::fold-left-list)`. New tests use `deftest` and are registered automatically.

## Architecture

- `package.lisp` defines the `FOLD` package (uses only `COMMON-LISP`) and its export list. Any new public function or variable must be added to `:export` there.
- `fold.lisp` holds all implementation. `fold.asd` lists components (`fold` depends on `package`); bump `:version` there on releases.
- `fold-left` dispatches on argument type inside a `labels`: lists, simple-strings and simple-vectors get specialized `(optimize (speed 3) (safety 0))` loops; other sequences and packages (external symbols) use generic paths. Multi-sequence (`-n`) variants are slower generic paths. `fold-right` is recursive and list-optimized.
- `alist-`, `plist-` and `hash-table-` folds are built on top of these primitives.

## Conventions

- `*truncate-fold*` controls error behavior: when true, dotted lists and mismatched sequence lengths are silently truncated; when nil, a continuable `cerror` is signaled. Keep this behavior in any new fold variant.
- When changing fold behavior, keep the README examples and API reference in sync.
- Files begin with `;;; -*- Lisp -*-` and use upper-case string package designators (`(in-package "FOLD")`).
