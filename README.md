# fold
FOLD-LEFT and FOLD-RIGHT

Examples:
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
```
> (fold-right (lambda (l r) `(F ,l ,r)) '(a b c) 'final)
(F A (F B (F C FINAL)))
```

Notes:

- Fold-left is iterative and should not blow the stack.
- Fold-left is optimized for lists, simple-strings and simple-vectors.
- Fold-left is unoptimized for mixed arguments of lists and other sequence types.

- Fold-right is recursive and may overflow the stack on long lists.
- Fold-right is optimized on lists.
- Fold-right is not optimized on other sequence types.
  
