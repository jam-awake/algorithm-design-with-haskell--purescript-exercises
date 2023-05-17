module Test.Chapter03.Code.BookSymList where

import Prelude

import Data.List (List)
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
type UnsafeSymmetricList a = Tuple (List a) (List a)

toList :: forall a. UnsafeSymmetricList a -> List a
toList (Tuple front back) = front <> List.reverse back
