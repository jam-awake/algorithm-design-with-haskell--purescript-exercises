module Test.Chapter03.Code.AdtSymList where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.SnocList (SnocList(..))

-- Since a `SymmetricList` is itself a list, why not use it to combine 
-- both the front/back lists? If we do that, we get the below implementation.
--
-- One can start to see the beginning of the Finger Tree data structure here.
-- in that the `a`s currently store only one value, but could be updated
-- to store 2 or 3 values in various contexts.
--
-- However, as seen below, this representation is just as slow as `List` due to stack-safety issues.
data AdtSymList a
  = Empty
  | Single a
  | Ends a (AdtSymList a) a

-- The above type is isomorphic to the below one.
-- Due to this being a recursive type, we need to use a newtype.
newtype AdtSymListIsomorphism1 a = AdtSymListIsomorphism1
  ( Either
      (Maybe a)
      { front :: a
      , middle :: AdtSymListIsomorphism1 a
      , back :: a
      }
  )

-- Here's another isomorphism that's a less readable.
newtype AdtSymListIsomorphism2 a = AdtSymListIsomorphism2
  ( Either
      (Maybe a)
      (Tuple (Tuple a a) (AdtSymListIsomorphism2 a))
  )

-- This is the simplest implementation of `toList`. However,
-- there are two problems with it.
-- 1. `toListStackUnsafe middle <> (last : Nil)` will traverse the
--     list created for `middle` twice.
--     After creating an initial list for `middle`, the list will be
--     reversed once (so that its last element is in head position)
--     and then append `last` will be appended onto that list
--     and then the resulting list will be reversed to get the original order back.
-- 2. the recursive call to `listStackUnsafe` is not in tail-call position,
--    so this will blow the stack on large inputs.
toListStackUnsafe :: forall a. AdtSymList a -> List a
toListStackUnsafe = case _ of
  Empty -> Nil
  Single a -> Cons a Nil
  Ends head middle last ->
    Cons head $ toListStackUnsafe middle <> (last : Nil)

-- Here's the stack-safe version with more verbosity to show how
-- the state is being changed over time.
-- To create a list in a performant way, we need 
--   1. to `cons` all the `Ends`' `last` values to the list
--   2. either `cons` the `Single` value or do nothing for the `Empty` value
--   3. `cons` all the `Ends`' `first` values to the list.
toListStackSafeVerbose :: forall a. AdtSymList a -> List a
toListStackSafeVerbose initialList = do
  let { heads, middles, list } = buildEndsStack { heads: SnocNil, list: Nil } initialList
  insertSnocs heads $ insertSnocs middles list
  where
  buildEndsStack
    :: { heads :: SnocList a, list :: List a }
    -> AdtSymList a
    -> { heads :: SnocList a, middles :: SnocList a, list :: List a }
  buildEndsStack acc = case _ of
    Empty -> { heads: acc.heads, middles: SnocNil, list: acc.list }
    Single a -> { heads: acc.heads, middles: Snoc SnocNil a, list: acc.list }
    Ends head middle last ->
      buildEndsStack { heads: Snoc acc.heads head, list: Cons last acc.list } middle

  insertSnocs :: SnocList a -> List a -> List a
  insertSnocs snocs ls = case snocs of
    SnocNil -> ls
    Snoc tail last -> insertSnocs tail (last : ls)

-- Same as `toListStackSafeVerbose` but with a few changes to decrease verbosity:
--   1. flip the args of `insertSnocs` so we can use `case _ of` rather than `case snocs of`
--   2. inline `insertSnocs` where `middles` is created as the value is produced and then immediately consumed.
toList :: forall a. AdtSymList a -> List a
toList initialList = buildEndsStack { heads: SnocNil, list: Nil } initialList
  where
  buildEndsStack
    :: { heads :: SnocList a, list :: List a }
    -> AdtSymList a
    -> List a
  buildEndsStack acc = case _ of
    Empty -> insertSnocs acc.list acc.heads
    Single a -> insertSnocs (Cons a acc.list) acc.heads
    Ends head middle last ->
      buildEndsStack { heads: Snoc acc.heads head, list: Cons last acc.list } middle

  insertSnocs :: List a -> SnocList a -> List a
  insertSnocs ls = case _ of
    SnocNil -> ls
    Snoc tail last -> insertSnocs (last : ls) tail

-- Notice how this list is self-balancing, but it comes at a cost.
-- `snoc` below is `O(n/2)` because of the recursive call to `snoc`.
-- On a list like `Ends 1 (Ends 2 (Ends 3 (Single 4) 5) 6) 7`,
-- `snoc` will traverse through half of the list's elements before returning.
-- Moreover, since it does not return until the recursive call completes,
-- this function is not stack-safe.
fromListStackUnsafe :: forall a. List a -> AdtSymList a
fromListStackUnsafe ls = do
  let
    snoc :: AdtSymList a -> a -> AdtSymList a
    snoc csl a = case csl of
      Empty -> Single a
      Single b -> Ends b Empty a
      Ends left middle right -> Ends left (snoc middle right) a

  foldl snoc Empty ls

-- Here's the same implementation as above except this call
-- is stack-safe by using an accumulator.
fromList :: forall a. List a -> AdtSymList a
fromList ls = do
  let
    -- Every `snoc` call is always at least `O(n)` because one must
    -- traverse through to the midpoint of the list when building the stack
    -- and then traverse the resulting stack to rebuild the Ends.
    -- Since the stack is half of the list, iterating through each half is
    -- equal to iterating through the entire list.
    -- Thus, this representation is no better than a normal `List`.
    snoc :: AdtSymList a -> a -> AdtSymList a
    snoc = buildStack Nil
      where
      buildStack :: List (Tuple a a) -> AdtSymList a -> a -> AdtSymList a
      buildStack endsStack listWithEnds a = case listWithEnds of
        Empty -> rebuildEnds endsStack $ Single a
        Single b -> rebuildEnds endsStack $ Ends b Empty a
        Ends left middle right ->
          -- Now we need to deal with stack-safety issues.
          -- We write `Ends left (snoc middle right) a`
          -- by storing the two end pieces, `left` and `a` in the `endsStack`.
          -- Once all `Ends` have been accounted for in this manner
          -- we will unroll this stack back to `Ends`.
          buildStack (Tuple left a : endsStack) middle right

      rebuildEnds :: List (Tuple a a) -> AdtSymList a -> AdtSymList a
      rebuildEnds endsStack middle = case endsStack of
        Nil -> middle
        (Tuple l r : rest) -> rebuildEnds rest $ Ends l middle r

  foldl snoc Empty ls