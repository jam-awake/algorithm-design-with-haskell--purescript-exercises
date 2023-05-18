module Test.Chapter03.Code.RandomAccessList where

import Prelude

import Data.Foldable (foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Test.Chapter03.Code.SnocList (SnocList(..), (<:))

data Tree a
  = Leaf a
  | Node Int (Tree a) (Tree a)

derive instance Eq a => Eq (Tree a)
derive instance Generic (Tree a) _

data Digit a
  = Zero
  | One (Tree a)

derive instance Eq a => Eq (Digit a)
derive instance Generic (Digit a) _

type RandomAccessList a = List (Digit a)

treeSize :: forall a. Tree a -> Int
treeSize = case _ of
  Leaf _ -> 1
  Node size _ _ -> size

node :: forall a. Tree a -> Tree a -> Tree a
node l r = Node (treeSize l + treeSize r) l r

-- 'a' : 'b' : 'c' : 'd' : 'e' : 'f' : Nil
abcdefExample :: RandomAccessList Char
abcdefExample =
  Zero
    : (One (Node 2 (Leaf 'a') (Leaf 'b')))
    :
      ( One
          ( Node 4
              (Node 2 (Leaf 'c') (Leaf 'd'))
              (Node 2 (Leaf 'e') (Leaf 'f'))
          )
      )
    : Nil

toList :: forall a. RandomAccessList a -> List a
toList ls = ls >>= digitToList

-- Ideally, this would be better thought-through
-- but for convenience, I'm not worrying about that here.
fromList :: forall a. List a -> RandomAccessList a
fromList = foldl (flip consRA) Nil <<< List.reverse

digitToList :: forall a. Digit a -> List a
digitToList = case _ of
  Zero -> Nil
  One t -> treeToList t

-- Note: this is stack safe via the two accumulators
treeToList :: forall a. Tree a -> List a
treeToList = go Nil Nil
  where
  go leftNodeStack acc = case _ of
    Leaf a -> do
      let next = Cons a acc
      case leftNodeStack of
        Nil -> next
        Cons h rest -> go rest next h
    Node _ l r ->
      go (l : leftNodeStack) acc r

index :: forall a. RandomAccessList a -> Int -> Maybe a
index ls idx = case ls of
  Nil -> Nothing
  Cons h tail -> case h of
    Zero -> index tail idx
    One t -> do
      let size = treeSize t
      if idx < size then indexTree t idx
      else index tail (idx - size)

indexTree :: forall a. Tree a -> Int -> Maybe a
indexTree t idx = case t of
  Leaf x
    | idx == 0 -> Just x
    | otherwise -> Nothing -- index is too large
  Node size l r -> do
    let m = size / 2
    if idx < m then indexTree l idx
    else indexTree r (idx - m)

isEmpty :: forall a. RandomAccessList a -> Boolean
isEmpty = List.null

-- Unlike the book, this is a stack-safe implementation
-- Hence the extra `zeroAcc` argument that accumulates the
-- `Zero`s that should be `Cons`'d onto the front of the
-- recursive result of the `consTree`'s `One` case.
consRA :: forall a. a -> RandomAccessList a -> RandomAccessList a
consRA a ls = consRA' SnocNil (Leaf a) ls
  where
  consRA' :: SnocList (Digit a) -> Tree a -> RandomAccessList a -> RandomAccessList a
  consRA' zeroAcc leaf = case _ of
    Nil -> foldr Cons ((One leaf) : Nil) zeroAcc
    Cons h tail -> consTree zeroAcc leaf h tail

  consTree :: SnocList (Digit a) -> Tree a -> Digit a -> RandomAccessList a -> RandomAccessList a
  consTree zeroAcc leaf h tail = case h of
    Zero -> foldr Cons ((One leaf) : Nil) zeroAcc
    One t -> consRA' (zeroAcc <: Zero) (node leaf t) tail

-- This is the implementation provided by the book. However, it is not stack-safe.
-- We'll fix that in the next iteration.
unconsRA_stackUnsafe :: forall a. RandomAccessList a -> Maybe { head :: a, tail :: RandomAccessList a }
unconsRA_stackUnsafe ls = case unconsT ls of
  Just { head: Leaf x, tail } -> Just { head: x, tail }
  _ -> Nothing
  where
  unconsT :: RandomAccessList a -> Maybe { head :: Tree a, tail :: RandomAccessList a }
  unconsT = case _ of
    Nil -> Nothing
    Cons (One head) tail -> Just { head, tail: if List.null tail then Nil else Zero : tail }
    Cons Zero tail ->
      -- Because we use the results of `unconstT` in `unconsT`,
      -- this function is not stack-safe.
      case unconsT tail of
        Just { head: Node _ l r, tail: ys } -> Just { head: l, tail: One r : ys }
        _ -> Nothing

-- Same as above, but with two changes:
-- 1. this is stack-safe
-- 2. to make code readable from top-to-bottom, various things were reordered:
--      1. in `unconsT`, the `Cons Zero tail` case was moved to the third case position to the first position
--      2. the initial entry into `unconsT` was moved to `doTheUncons`, which appears at the end of this
--         identifier's block of code.
unconsRA_stackSafeVerbose :: forall a. RandomAccessList a -> Maybe { head :: a, tail :: RandomAccessList a }
unconsRA_stackSafeVerbose = doTheUncons
  where
  -- Note: this function's 3 cases have had their order changed. The third case in the original stack-unsafe version
  -- was the third case but below it's the first case. This is done so that the cases read in the order. Normally,
  -- the third case would remain the third case.
  unconsT
    :: List ({ head :: Tree a, tail :: RandomAccessList a } -> Maybe { head :: Tree a, tail :: RandomAccessList a })
    -> RandomAccessList a
    -> Maybe { head :: Tree a, tail :: RandomAccessList a }
  unconsT unconsNodeStack = case _ of
    Cons Zero tail ->
      -- First, we realize that the result of calling `unconsT` is passed back into a function.
      -- In other words, that function acts like a callback. We have defined that callback separately below
      -- via `unconsNode`. By saving that callback onto a stack, we can later apply that callback to the value it receives
      -- in a later step in our recursive loop.
      unconsT (unconsNode : unconsNodeStack) tail
    Nil ->
      -- Second, if we hit the `Nil` case, we've iterated through the entire list and never came across
      -- the real "base case" of the recursive function. So, we just return Nothing here.
      -- There's no need to even touch the stack.
      Nothing
    Cons (One head) tail -> do
      -- Third, we produce the real "base case" of the recursive function.
      let baseCaseValue = { head, tail: if List.null tail then Nil else Zero : tail }
      -- Fourth, we can pass the "base case" value into the stack of callbacks functions.
      --
      -- Since the returned value of each callback function is `Maybe a`, a monadic value,
      -- we could use `foldM` to apply the callback function to the value recursively until
      -- we have finished traversing through the entire stack. In other words,
      -- we could write the following:
      --
      --    foldM (\mbValuse unconsNode' -> mbValue >>= unconsNode') (Just baseCaseValue) acc
      --
      -- However, `foldM` does not short-circuit. So, even if the first callback function
      -- returns a `Nothing`, we will still traverse the entire stack, 
      -- only to throw away the result each time due to returning `Nothing` on each case.
      -- Rather than paying for this performance cost, we write a helper function
      -- to add the short-circuiting to `foldM` since the `Foldable` type here is a `List`.
      foldMShortCircuit baseCaseValue unconsNodeStack

  unconsNode
    :: { head :: Tree a, tail :: RandomAccessList a }
    -> Maybe { head :: Tree a, tail :: RandomAccessList a }
  unconsNode { head, tail } = case head of
    Node _ l r -> Just { head: l, tail: One r : tail }
    _ -> Nothing

  -- Same as `foldM` but with short-circuiting.
  foldMShortCircuit :: forall x. x -> List (x -> Maybe x) -> Maybe x
  foldMShortCircuit baseCaseValue = case _ of
    Nil -> Just baseCaseValue
    Cons fn tail -> case fn baseCaseValue of
      Nothing -> Nothing
      Just newBaseCase -> foldMShortCircuit newBaseCase tail

  -- Finally, we call `unconsT` with an initially-empty stack, casing on the result
  -- as before. 
  doTheUncons :: RandomAccessList a -> Maybe { head :: a, tail :: RandomAccessList a }
  doTheUncons ls = case unconsT Nil ls of
    Just { head: Leaf x, tail } -> Just { head: x, tail }
    _ -> Nothing

-- Here's the same stack-safe code as above but with everything in its proper order and position.
unconsRA :: forall a. RandomAccessList a -> Maybe { head :: a, tail :: RandomAccessList a }
unconsRA ls = case unconsT Nil ls of
  Just { head: Leaf x, tail } -> Just { head: x, tail }
  _ -> Nothing
  where
  unconsT
    :: List ({ head :: Tree a, tail :: RandomAccessList a } -> Maybe { head :: Tree a, tail :: RandomAccessList a })
    -> RandomAccessList a
    -> Maybe { head :: Tree a, tail :: RandomAccessList a }
  unconsT unconsNodeStack = case _ of
    Nil -> Nothing
    Cons (One head) tail -> foldMShortCircuit { head, tail: if List.null tail then Nil else Zero : tail } unconsNodeStack
    Cons Zero tail -> unconsT (unconsNode : unconsNodeStack) tail

  unconsNode
    :: { head :: Tree a, tail :: RandomAccessList a }
    -> Maybe { head :: Tree a, tail :: RandomAccessList a }
  unconsNode { head, tail } = case head of
    Node _ l r -> Just { head: l, tail: One r : tail }
    _ -> Nothing

  foldMShortCircuit :: forall x. x -> List (x -> Maybe x) -> Maybe x
  foldMShortCircuit baseCaseValue = case _ of
    Nil -> Just baseCaseValue
    Cons fn tail -> case fn baseCaseValue of
      Nothing -> Nothing
      Just newBaseCase -> foldMShortCircuit newBaseCase tail
