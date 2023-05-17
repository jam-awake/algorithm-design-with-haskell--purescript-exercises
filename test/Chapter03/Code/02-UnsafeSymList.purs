module Test.Chapter03.Code.UnsafeSymList where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as List

-- The first change we'll make is to label
-- what each value is to make it more readable.
type UnsafeSymmetricList a =
  { front :: List a
  , back :: List a
  }

toList :: forall a. UnsafeSymmetricList a -> List a
toList { front, back } = front <> List.reverse back

fromList :: forall a. List a -> UnsafeSymmetricList a
fromList ls = do
  let
    midPoint = (List.length ls) / 2
    result = ls # flip foldlWithIndex { front: Nil, back: Nil } \idx acc next ->
      if idx <= midPoint then
        acc { front = next : acc.front }
      else
        acc { back = next : acc.back }
  result { front = List.reverse result.front }
