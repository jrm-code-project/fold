# fold
FOLD-LEFT and FOLD-RIGHT

```
> (fold-left (lambda (l r) `(F ,l ,r)) 'i '(a b c))
(F (F (F I A) B) C)
```
```
> (fold-right (lambda (l r) `(F ,l ,r)) '(a b c) 'f)
(F A (F B (F C F)))
```
