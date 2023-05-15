# Exercise 10

`foldl op e ls == foldr op e xs` when 
- `op` has the type `forall a. a -> a -> a` and 
- `op` is commutative: `a op (b op c) == (a op b) op c`.
