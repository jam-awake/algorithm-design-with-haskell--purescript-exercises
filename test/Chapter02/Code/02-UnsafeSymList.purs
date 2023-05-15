module Test.Chapter02.Code.UnsafeSymList where

import Prelude

import Data.List (List)
import Data.List as List

-- The first change we'll make is to label
-- what each value is to make it more readable.
type UnsafeSymmetricList a =
  { front :: List a
  , back :: List a
  }

toList :: forall a. UnsafeSymmetricList a -> List a
toList { front, back } = front <> List.reverse back
