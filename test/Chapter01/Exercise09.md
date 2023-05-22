# Exercise 9

```purs
foldl f e xs = if null xs then e else f (foldl f e (init xs)) (last xs)
```

While this function produces the same value as the correct `foldl`...

```purs
foldl (\a b -> a + b) 0 (1 : 2 : 3 : Nil)
(foldl (_ + 1) 0 (1 : 2 : Nil)) + 3
(foldl (_ + 1) 0 (1 : Nil)) + 2 + 3
(foldl (_ + 1) 0 (Nil)) + 1 + 2 + 3
0 + 1 + 2 + 3
```

... the issue is that it builds a copy of the list (minus 1 element) on each recursive step, making it inefficient.
