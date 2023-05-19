module Test.Chapter01.Exercise12 where

import Prelude

import Data.Eq (class Eq1)
import Data.List (foldr)
import Data.List as List
import Data.List.Types (List(..), (:))
import Data.List.Types as NEL
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Test.Chapter01.Exercise12.SnocList (SnocList(..), (<:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Note: while this function is stack-safe, it is NOT heap-safe.
--
-- Given a list of 4 elements from 1 to 4, we're effectively creating the following list:
--    Nil
--      : (1 : Nil)
--      : (1 : 2 : Nil)
--      : (1 : 2 : 3 : Nil)
--      : (1 : 2 : 3 : 4 : Nil)
--
-- Using the `1` element as an example, notice how it always appears in the `head` position
-- of each list. This means the `1` element is NOT shared between the four lists.
-- Rather, a new allocation is made for the `1` element every time. Thus,
-- the resulting lists are not persistent.
--
-- In other words, given a list with `n` elements, the number of allocations made is
-- the same as `factorial(n)` except we use `+` rather than `*`:
--
--   numOfAllocations n = if n == 0 then 0 else n + numOfAllocations (n - 1)
--
-- Or put differently, if we had a list of 10 elements from 1 to 10, 
-- we would make the following number of allocations:
--
--        10     -- `1` is allocated `10 - 0` times
--         9     -- `2` is allocated `10 - 1` times
--         8     -- `3` is allocated `10 - 2` times
--         7     -- `4` is allocated `10 - 3` times
--         6     -- `5` is allocated `10 - 4` times
--         5     -- `6` is allocated `10 - 5` times
--         4     -- `7` is allocated `10 - 6` times
--         3     -- `8` is allocated `10 - 7` times
--         2     -- `9` is allocated `10 - 8` times
--     +   1     -- `10` is allocated `10 - 19` times
--     ----------------------------------------------
--        55 total allocations
--
-- Or, one can visualize this calculation by adding up all 10s and then the remaining number
--
--              1    2    3    4   5
--     +  10    9    8    7    6
--     -----------------------------
--        10 + 10 + 10 + 10 + 10 + 5  ==  5 * 10 + 5  ==  55
--   
inits_stackSafeHeapUnsafe :: forall a. List a -> NonEmpty List (List a)
inits_stackSafeHeapUnsafe = go Nil Nil
  where
  go :: List (List a) -> List a -> List a -> NonEmpty List (List a)
  go acc lastInit = case _ of
    Nil -> Nil :| reverseInnards Nil acc
    Cons h t -> do
      let nextInit = Cons h lastInit
      go (Cons nextInit acc) nextInit t

  reverseInnards :: List (List a) -> List (List a) -> List (List a)
  reverseInnards acc = case _ of
    Nil -> acc
    Cons h t -> reverseInnards (Cons (List.reverse h) acc) t

-- `NonEmpty  f a = NonEmpty  a (f a)  ` - Good for `List`s where `head` is exposed
-- `NonEmptyR f a = NonEmptyR   (f a) a` - Good for `SnocList`s where `last` is exposed
data NonEmptyR f a = NonEmptyR (f a) a

infixl 5 NonEmptyR as |:

derive instance (Eq1 f, Eq a) => Eq (NonEmptyR f a)
derive instance Eq1 f => Eq1 (NonEmptyR f)
instance (Show (f a), Show a) => Show (NonEmptyR f a) where
  show (NonEmptyR fa a) = "(NonEmptyR " <> show fa <> " |: " <> show a <> ")"

-- This implementation is both stack-safe and heap-safe and easier to read than the above.
--  - Stack-safety: `go` is always called in a tail-call position.
--  - Heap-safety: by using `SnocList`, the `1` element is in the tail position,
--      which means it can be reused amongst all the lists after that point.
--      Thus, this implementation utilizes persistence.
--
-- To indicate non-emptyness, we use `NonEmptyR` rather than `NonEMpty`
-- because `NonEmpty`'s `Show` instance assumes a left-to-right ordering:
--   - Lists are correct:   `show $ NonEmpty 1 (2 : Nil)` == "1 :| 2 : Nil"
--   - SnocLists are not:   `show $ NonEmpty 1 (SnocNil <: 2)` == "1 :| SnocNil <: 2"
--   - SnocLists should be: `show $ NonEmptyR (SnocNil <: 2) 1` == "SnocNil <: 2 |: 1"
--
-- Given a list of 4 elements from 1 to 4, we're effectively creating the following list:
--    SnocNil
--      <: (SnocNil <: 1)
--      <: (SnocNil <: 1 <: 2)
--      |: (SnocNil <: 1 <: 2 <: 3)
inits' :: forall a. List a -> NonEmptyR SnocList (SnocList a)
inits' = go SnocNil SnocNil
  where
  go :: SnocList (SnocList a) -> SnocList a -> List a -> NonEmptyR SnocList (SnocList a)
  go initRuns prevRun = case _ of
    Nil ->
      initRuns |: prevRun
    Cons h t -> do
      go (initRuns <: prevRun) (Snoc prevRun h) t

-- Note that in `inits`, the returned outer list is still `SnocList`.
-- If we want the closest we can get to `inits`, we can reverse the outer `SnocList`
-- without running into heap-safety issues. If we reverse the inner `SnocList`,
-- that's when we run into heap-safety issues.
inits :: forall a. List a -> NonEmpty List (SnocList a)
inits = toNonEmptyList <<< inits'

toNonEmptyList :: forall a. NonEmptyR SnocList a -> NonEmpty List a
toNonEmptyList (i |: l) = unwrap $ foldr NEL.nelCons (pure l) i

-- Below is the code we would write for a stack-safe, heap-safe `tails`.
-- It's actually easier to implement a version that returns back a `SnocList`
-- via `tails'` and then just convert that `NonEmptyR SnocList` into a `NonEmpty List`.
--
-- Given a list `1 : 2 : 3 : 4 : Nil`, we will return the following list:
--
--    (1 : 2 : 3 : 4 : Nil)
--      :| (2 : 3 : 4 : Nil)
--       : (3 : 4 : Nil)
--       : (4 : Nil)
--       : (Nil :: List Int)          -- annotations added here for clarity
--       : (Nil :: List (List Int))
--
-- 
tails :: forall a. List a -> NonEmpty List (List a)
tails = toNonEmptyList <<< tails'

tails' :: forall a. List a -> NonEmptyR SnocList (List a)
tails' = go SnocNil
  where
  go :: SnocList (List a) -> List a -> NonEmptyR SnocList (List a)
  go tailRuns ls = case ls of
    Nil -> tailRuns |: ls
    Cons _ t -> go (tailRuns <: ls) t

spec :: Spec Unit
spec = describe "Exercise 12" do
  describe "inits_stackSafeHeapUnsafe" do
    it "works on empty lists" do
      (inits_stackSafeHeapUnsafe (Nil :: List Int)) `shouldEqual`
        ( (Nil :: List Int)
            :| Nil
        )
    it "works on small inputs" do
      (inits_stackSafeHeapUnsafe (1 : 2 : 3 : 4 : Nil)) `shouldEqual`
        ( (Nil :: List Int)
            :| (1 : Nil)
              : (1 : 2 : Nil)
              : (1 : 2 : 3 : Nil)
              : (1 : 2 : 3 : 4 : Nil)
              : Nil
        )
  describe "inits (stack-safe; heap-safe; outer list is SnocList)" do
    it "works on empty lists" do
      (inits' (Nil :: List Int)) `shouldEqual`
        ( SnocNil
            |: (SnocNil :: SnocList Int)
        )
    it "works on small inputs" do
      (inits' (1 : 2 : 3 : 4 : Nil)) `shouldEqual`
        ( (SnocNil :: SnocList (SnocList Int))
            <: (SnocNil :: SnocList Int)
            <: (SnocNil <: 1)
            <: (SnocNil <: 1 <: 2)
            <: (SnocNil <: 1 <: 2 <: 3)
            |: (SnocNil <: 1 <: 2 <: 3 <: 4)
        )
  describe "inits (stack-safe; heap-safe; outer list is List)" do
    it "works on empty lists" do
      (inits (Nil :: List Int)) `shouldEqual`
        ( (SnocNil :: SnocList Int)
            :| Nil
        )
    it "works on small inputs" do
      (inits (1 : 2 : 3 : 4 : Nil)) `shouldEqual`
        ( (SnocNil :: SnocList Int)
            :| (SnocNil <: 1)
              : (SnocNil <: 1 <: 2)
              : (SnocNil <: 1 <: 2 <: 3)
              : (SnocNil <: 1 <: 2 <: 3 <: 4)
              : Nil
        )
  describe "tails" do
    it "works on empty inputs" do
      (tails Nil) `shouldEqual`
        ( (Nil :: List Int)
            :| Nil
        )
    it "works on small inputs" do
      (tails (1 : 2 : 3 : 4 : Nil)) `shouldEqual`
        ( (1 : 2 : 3 : 4 : Nil)
            :| (2 : 3 : 4 : Nil)
              : (3 : 4 : Nil)
              : (4 : Nil)
              : (Nil :: List Int)
              : (Nil :: List (List Int))
        )
  describe "tails'" do
    it "works on empty inputs" do
      (tails' Nil) `shouldEqual`
        ( SnocNil
            |: (Nil :: List Int)
        )
    it "works on small inputs" do
      (tails' (1 : 2 : 3 : 4 : Nil)) `shouldEqual`
        ( SnocNil
            <: (1 : 2 : 3 : 4 : Nil)
            <: (2 : 3 : 4 : Nil)
            <: (3 : 4 : Nil)
            <: (4 : Nil)
            |: (Nil :: List Int)
        )
