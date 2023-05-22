# Exercise 1

```purs
maximum :: forall a. Ord a => List a -> Maybe a

minimum :: forall a. Ord a => List a -> Maybe a

take :: forall a. Int -> List a -> List a
drop :: forall a. Int -> List a -> List a

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile :: forall a. (a -> Boolean) -> List a -> List a

inits :: forall a. List a -> List (List a)
tails :: forall a. List a -> List (List a)

splitAt :: forall a. Int -> List a -> { before :: List a, after :: List a }

span :: forall a. Int -> Int -> List a -> Maybe (List a)

null :: forall a. List a -> Boolean
elem :: forall a. a -> List a -> Boolean
infixr 4 elem as !!

zipWith :: forall a b. (a -> b -> c) -> List a -> List b -> List (Tuple a b)

all :: forall a. (a -> Boolean) -> List a -> Boolean
```
