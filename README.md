# fold
`FOLD-LEFT` and `FOLD-RIGHT`

## Installation

 - Clone this repository into `~/quicklisp/local-projects/fold/`

 - `(ql:quickload "fold")`

 - In your package definitions, add "FOLD" to the used packages list.

---

## Examples

### Fold-Left
```
;; Fold-left a function over a list.
> (fold-left (lambda (l r) `(F ,l ,r)) 'i '(a b c))
(F (F (F I A) B) C)

;; Fold-left can take multiple lists.
> (fold-left (lambda (acc one two) `(F ,acc ,one ,two)) 'i '(a b c) '(do re mi))
(F (F (F I A DO) B RE) C MI)

;; The lists can be strings or vectors.
> (fold-left #'list nil "foo" "bar")
(((NIL #\f #\b) #\o #\a) #\o #\r)

> (fold-left #'list nil #(a b c))
(((NIL A) B) C)

;; The sequences to fold over need not be the same type.
> (fold-left #'list nil '(a b c) "foo")
(((NIL A #\f) B #\o) C #\o)

;; Dotted lists give a continuable error
> (fold-left #'list nil '(a b . c))
Continuable error

;; If *truncate-fold* is t, dotted lists are ignored.
> (let ((*truncate-fold* t))
    (fold-left #'list nil '(a b . c)))
((NIL A) B)

;; fold-left works on non-simple vectors
> (fold-left #'list nil (make-array :adjustable t :initial-contents '(a b c)))
(((NIL A) B) C)
```

### Fold-Right
```
> (fold-right (lambda (l r) `(F ,l ,r)) '(a b c) 'final)
(F A (F B (F C FINAL)))
```

---

## API Reference

### `fold-left`
- **Description**: Iteratively applies a function to elements of one or more sequences from left to right.
- **Signature**: `(fold-left function initial-value &rest sequences)`
- **Parameters**:
  - `function`: A function to apply. It must accept as many arguments as there are sequences plus one for the accumulator.
  - `initial-value`: The initial value of the accumulator.
  - `sequences`: One or more sequences to fold over.
- **Returns**: The final accumulated value.

### `fold-right`
- **Description**: Recursively applies a function to elements of one or more sequences from right to left.
- **Signature**: `(fold-right function &rest sequences-and-final)`
- **Parameters**:
  - `function`: A function to apply. It must accept as many arguments as there are sequences plus one for the accumulator.
  - `sequences-and-final`: One or more sequences to fold over followed by the final value.
- **Returns**: The final accumulated value.

---

## Error Handling

- **Dotted Lists**: Folding over dotted lists raises a continuable error. To ignore the dotted portion, set `*truncate-fold*` to `t`.
  ```
  > (fold-left #'list nil '(a b . c))
  Continuable error

  > (let ((*truncate-fold* t))
      (fold-left #'list nil '(a b . c)))
  ((NIL A) B)
  ```

- **Mismatched Sequence Lengths**: Folding over sequences of different lengths raises an error unless `*truncate-fold*` is `t`.

---

## Performance Notes

- **Fold-Left**:
  - Optimized for lists, simple-strings, and simple-vectors.
  - Unoptimized for mixed arguments of lists and other sequence types.
  - Does not blow the stack due to its iterative nature.

- **Fold-Right**:
  - Optimized for lists.
  - Not optimized for other sequence types.
  - Recursive and may overflow the stack on long lists.

---

## Contributing

1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Write tests for your changes.
4. Submit a pull request with a detailed description of your changes.

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

