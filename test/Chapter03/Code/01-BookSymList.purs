module Test.Chapter03.Code.BookSymList where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Tuple (Tuple(..))

-- This is how the book defines the type, but
-- there are a few problems with it
-- 1. It's not type-safe as there are no guarantees enforced by the type system regarding
--    the order of the values.
-- 2. If we newtyped the below type and derived a Foldable instance,
--    the resulting instance would not work as expected as
--    the fold would fold from the start of the list
--    to the middle, and then from the end of the list
--    to the middle
type BookSymList a = Tuple (List a) (List a)

toList :: forall a. BookSymList a -> List a
toList (Tuple front back)
  -- Note: it seems like there is no faster way to write this
  = front <> List.reverse back {-
  = front <> foldl (flip Cons) Nil back
  = foldl (flip Cons) (foldl (flip Cons) Nil back) $ List.reverse front
  = foldl (flip Cons) (foldl (flip Cons) Nil back) (foldl (flip Cons) Nil front)
  -- We have to reverse the `back` list
  -- then reverse the `front` list so as to put its last element in the `head` position
  -- and then fold over the reversed `front` list so as to append its elements
  -- on top of the `back` list.
  -}

-- Notice that we traverse the list 2.5 times:
-- 1. getting the length, so we can determine what its midpoint is
-- 2. creating the initial front/back values
-- 3. reversing the front.
fromList :: forall a. List a -> BookSymList a
fromList ls = do
  let
    midPoint = (List.length ls) / 2
    { front, back } = ls # flip foldlWithIndex { front: Nil, back: Nil } \idx acc next ->
      if idx <= midPoint then
        acc { front = next : acc.front }
      else
        acc { back = next : acc.back }
  Tuple (List.reverse front) back
