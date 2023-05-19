module Test.Chapter03.Code.SafishSymList where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List)
import Data.List as List
import Test.Chapter01.Exercise12.SnocList (SnocList(..))
import Test.Chapter01.Exercise12.SnocList as SnocList

-- Using `SnocList` for the `back` value's type
-- makes the desired structure more obvious
-- than just using another `List`.
-- Moreover, if we newtyped this, 
-- we can now derive a correct `Foldable` instance.
type UnsafeSymmetricList a =
  { front :: List a
  , back :: SnocList a
  }

toList :: forall a. UnsafeSymmetricList a -> List a
toList { front, back } = front <> SnocList.toList back

-- Notice that we traverse the list 2.5 times:
-- 1. getting the length, so we can determine what its midpoint is
-- 2. creating the initial front/back values
-- 3. reversing the front.
fromList :: forall a. List a -> UnsafeSymmetricList a
fromList ls = do
  let
    midPoint = (List.length ls) / 2
    { front, back } = ls # flip foldlWithIndex { front: SnocNil, back: SnocNil } \idx acc next ->
      if idx <= midPoint then
        acc { front = Snoc acc.front next }
      else
        acc { back = Snoc acc.back next }
  { front: SnocList.toList front, back }