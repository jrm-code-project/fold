# fold
FOLD-LEFT and FOLD-RIGHT

```
> (fold:fold-left (lambda (l r) `(F ,l ,r)) 'i '(a b c))
(F (F (F I A) B) C)
```
