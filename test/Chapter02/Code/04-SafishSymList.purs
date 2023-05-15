module Test.Chapter02.Code.SafishSymList where

import Prelude

import Data.List (List)
import Test.Chapter02.Code.SnocList (SnocList)
import Test.Chapter02.Code.SnocList as SnocList

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
